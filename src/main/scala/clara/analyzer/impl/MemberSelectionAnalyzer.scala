package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{DefId, TypeConId, Terms, Types}

import clara.util.Safe._

object MemberSelectionAnalyzerImpl {
  sealed trait CandidateMember
  case class FieldCandidate(typ: Types.Type) extends CandidateMember
  case class MethodCandidate(method: EnvMethod, receivers: Seq[Receiver]) extends CandidateMember

  case class Receiver(objectType: Types.Type, targetType: Types.Type)
  case class Candidate(member: CandidateMember, unboxCount: Int)
}

case class MemberSelectionAnalyzerImpl(env: Env, name: String, memberPos: Pos) {
  import MemberSelectionAnalyzerImpl._

  def memberSelection(objectTerm: Terms.ValueExpr): An[Terms.MemberSelection] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    memberOfType(objectTerm.typ, Set.empty).flatMap(An.fromSomeOrError(_, memberNotFound)).flatMap { candidate =>
      selectedMember(objectTerm.typ, candidate.member).map { case (member, typ) =>
        Terms.MemberSelection(objectTerm, candidate.unboxCount, name, member, typ)
      }
    }
  }

  def memberOfType(objectType: Types.Type, unwrapped: Set[TypeConId]): An[Option[Candidate]] = objectType match {
    case Types.ErrorType => An.valid(Some(Candidate(FieldCandidate(Types.ErrorType), 0)))
    case Types.Record(fields) => An.valid(fields.get(name).map(typ => Candidate(FieldCandidate(typ), 0)))
    case union: Types.Union => memberOfUnion(union, unwrapped)
    case Types.Intersection(parts) if parts.contains(Types.ErrorType) => memberOfType(Types.ErrorType, unwrapped)
    case intersection: Types.Intersection => memberOfIntersection(intersection, unwrapped)
    case t: Types.Alias => TypeInterpreter.wrappedType(t).flatMap(memberOfType(_, unwrapped))
    case t: Types.Tagged => methodOrWrappedMember(t, unwrapped)
    case t: Types.Boxed => methodOrWrappedMember(t, unwrapped)
    case t: Types.Opaque => method(t)
    case t: Types.Singleton => method(t)
    case _ => An.valid(None)
  }

  def memberOfUnion(union: Types.Union, unwrapped: Set[TypeConId]): An[Option[Candidate]] =
    An.seq(union.types.map(memberOfType(_, unwrapped))).flatMap { members =>
      val candidates = union.types.zip(members).collect {
        case (alternative, Some(candidate)) if alternative !== Types.ErrorType => candidate
      }
      lazy val differentMembers = SourceMessage(memberPos,
        safe"`$name` is a different member in the alternatives of `${Types.toSource(union)}`, which isn't supported yet"
      )

      if (members.contains(None)) An.valid(None)
      else candidates.distinctBy(candidateKey) match {
        case Seq() => memberOfType(Types.ErrorType, unwrapped)
        case Seq(_) => An.valid(Some(mergedCandidate(candidates)))
        case _ => An.error(differentMembers)
      }
    }

  def candidateKey(candidate: Candidate): (Option[DefId], Int) = {
    val member = candidate.member match {
      case _: FieldCandidate => None
      case MethodCandidate(method, _) => Some(method.defId)
    }

    (member, candidate.unboxCount)
  }

  def mergedCandidate(candidates: Seq[Candidate]): Candidate = {
    val member = candidates.map(_.member) match {
      case (first: MethodCandidate) +: _ => MethodCandidate(first.method, candidates.flatMap(_.member match {
        case MethodCandidate(_, receivers) => receivers
        case _: FieldCandidate => Nil
      }))
      case members => FieldCandidate(Types.Union(members.collect { case FieldCandidate(typ) => typ }))
    }

    Candidate(member, candidates.head.unboxCount)
  }

  def memberOfIntersection(intersection: Types.Intersection, unwrapped: Set[TypeConId]): An[Option[Candidate]] = {
    lazy val severalMembers = SourceMessage(memberPos,
      safe"`$name` is a member of several parts of `${Types.toSource(intersection)}`, which isn't supported yet"
    )

    An.seq(intersection.types.map(memberOfType(_, unwrapped))).map(_.flatten).flatMap {
      case Seq() => An.valid(None)
      case Seq(candidate) => An.valid(Some(candidate))
      case _ => An.error(severalMembers)
    }
  }

  def methodOrWrappedMember(objectType: Types.Wrapper, unwrapped: Set[TypeConId]): An[Option[Candidate]] =
    method(objectType).flatMap {
      case None if !unwrapped(objectType.con.tcId) =>
        val nextUnwrapped = unwrapped + objectType.con.tcId
        val wrappedCandidate = TypeInterpreter.wrappedType(objectType).flatMap(memberOfType(_, nextUnwrapped))

        wrappedCandidate.map(_.map { candidate =>
          objectType match {
            case _: Types.Boxed => candidate.copy(unboxCount = candidate.unboxCount + 1)
            case _ => candidate
          }
        })
      case candidate => An.valid(candidate)
    }

  def method(objectType: Types.Nominal): An[Option[Candidate]] = env.method(objectType.con, name).map { method =>
    val receivers = Seq(Receiver(objectType, method.targetType))

    sectionTypeArgs(method, receivers).map(_.map(_ => Candidate(MethodCandidate(method, receivers), 0)))
  }.getOrElse(An.valid(None))

  def sectionTypeArgs(method: EnvMethod, receivers: Seq[Receiver]): An[Option[Map[TypeConId, Types.Type]]] = {
    val sectionTypeParamIds = method.sectionTypeParams.map(_.tcId)

    An.seq(receivers.map { case Receiver(objectType, targetType) =>
      TypeInterpreter.findSubstitutions(sectionTypeParamIds.toSet, targetType, objectType)
    }).flatMap { substitutions =>
      val sectionTypeArgs = sectionTypeParamIds.map(id => id -> Types.Union(substitutions.flatMap(_.get(id)))).toMap

      An.seq(receivers.map { case Receiver(objectType, targetType) =>
        TypeInterpreter.isAssignable(objectType, Types.substituteParams(sectionTypeArgs, targetType))
      }).map(fits => if (fits.forall(identity)) Some(sectionTypeArgs) else None)
    }
  }

  def selectedMember(objectType: Types.Type, candidate: CandidateMember): An[(Terms.SelectedMember, Types.Type)] =
    candidate match {
      case FieldCandidate(typ) => An.valid((Terms.SelectedField, typ))
      case MethodCandidate(method, receivers) =>
        lazy val section = safe"::methods${method.sectionTypeParams.map(_.name).safeString("<", ", ", ">")}"
        lazy val noTypeArgs = SourceMessage(memberPos,
          safe"`$name` can't be selected from `${Types.toSource(objectType)}`, " +
            safe"no type arguments of `$section` fit all of its alternatives"
        )

        sectionTypeArgs(method, receivers).flatMap(An.fromSomeOrError(_, noTypeArgs)).flatMap { sectionTypeArgs =>
          val term = Terms.SelectedMethod(method.defId, method.definedInNs, method.attributes)

          An.valid((term, Types.substituteParams(sectionTypeArgs, method.typ))).tellUsage(method.defId, memberPos)
        }
    }
}

object MemberSelectionAnalyzer {
  def memberSelection(env: Env, objectTerm: Terms.ValueExpr, name: String, memberPos: Pos): An[Terms.MemberSelection] =
    MemberSelectionAnalyzerImpl(env, name, memberPos).memberSelection(objectTerm)
}
