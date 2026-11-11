package clara.analyzer.impl

import clara.ast.{SourceMessage, Pos}
import clara.asg.{Terms, TypeCons, Types}

import clara.util.Safe._

case class MemberSelectionAnalyzerImpl(env: Env, name: String, memberPos: Pos) {
  def memberSelection(objectTerm: Terms.ValueExpr): An[(Terms.SelectedMember, Types.Type)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    memberOfType(objectTerm.typ).flatMap(memberOpt => An.fromSomeOrError(memberOpt, memberNotFound))
  }

  def memberOfType(objectType: Types.Type): An[Option[(Terms.SelectedMember, Types.Type)]] = objectType match {
    case Types.ErrorType => An.valid(Some((Terms.SelectedField, Types.ErrorType)))
    case Types.Record(fields) => An.valid(fields.get(name).map(typ => (Terms.SelectedField, typ)))
    case union: Types.Union => memberOfUnion(union)
    case Types.Intersection(parts) if parts.contains(Types.ErrorType) => memberOfType(Types.ErrorType)
    case intersection: Types.Intersection => memberOfIntersection(intersection)
    case t: Types.Alias => TypeInterpreter.wrappedType(t).flatMap(memberOfType)
    case t: Types.Nominal => methodOfType(t)
    case _ => An.valid(None)
  }

  def memberOfUnion(union: Types.Union): An[Option[(Terms.SelectedMember, Types.Type)]] =
    An.seq(union.types.map(memberOfType)).flatMap { members =>
      val selectedMembers = union.types.zip(members).collect {
        case (alternative, Some((selectedMember, _))) if alternative !== Types.ErrorType => selectedMember
      }.distinctBy {
        case Terms.SelectedField => None
        case method: Terms.SelectedMethod => Some(method.defId)
      }
      lazy val differentMembers = SourceMessage(memberPos,
        safe"`$name` is a different member in the alternatives of `${Types.toSource(union)}`, which isn't supported yet"
      )

      if (members.contains(None)) An.valid(None)
      else selectedMembers match {
        case Seq(selectedMember) => An.valid(Some((selectedMember, Types.Union(members.flatten.map(_._2)))))
        case _ => An.error(differentMembers)
      }
    }

  def memberOfIntersection(intersection: Types.Intersection): An[Option[(Terms.SelectedMember, Types.Type)]] = {
    lazy val severalMembers = SourceMessage(memberPos,
      safe"`$name` is a member of several parts of `${Types.toSource(intersection)}`, which isn't supported yet"
    )

    An.seq(intersection.types.map(memberOfType)).map(_.flatten).flatMap {
      case Seq() => An.valid(None)
      case Seq(member) => An.valid(Some(member))
      case _ => An.error(severalMembers)
    }
  }

  def methodOfType(objectType: Types.Nominal): An[Option[(Terms.SelectedMethod, Types.Type)]] = objectType match {
    case _: Types.Param | _: Types.Alias | _: Types.Singleton => An.valid(None)
    case t: Types.Tagged => method(t.con, t.typeArgs)
    case t: Types.Boxed => method(t.con, t.typeArgs)
    case t: Types.Opaque => method(t.con, t.typeArgs)
  }

  def method(con: TypeCons.TypeCon, typeArgs: Seq[Types.Type]): An[Option[(Terms.SelectedMethod, Types.Type)]] =
    env.method(con, name) match {
      case None => An.valid(None)
      case Some(EnvMethod(attributes, typeParams, typ, defId, definedInNs)) =>
        val term = Terms.SelectedMethod(defId, definedInNs, attributes)
        val typeInst = Types.substituteParams(typeParams.map(_.tcId).zip(typeArgs).toMap, typ)

        An.valid(Some((term, typeInst))).tellUsage(defId, memberPos)
    }
}

object MemberSelectionAnalyzer {
  def memberSelection(env: Env, objectTerm: Terms.ValueExpr, name: String, memberPos: Pos) =
    MemberSelectionAnalyzerImpl(env, name, memberPos).memberSelection(objectTerm)
}
