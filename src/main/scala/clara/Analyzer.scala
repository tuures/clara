package clara

import collection.immutable.HashMap

import ai.x.safe._

object Analyzer {

  type Errors = Seq[String]

  sealed trait Type {
    def isSubTypeOf(other: Type): Boolean
    def getMember(name: String): Option[ValueMember]
    def toSource(env: Env): String
  }

  sealed trait ValueMember {
    def t: Type
  }
  sealed trait ConcreteValueMember extends ValueMember
  case class AbstractValueMember(t: Type) extends ValueMember
  case class PlainValueMember(t: Type) extends ConcreteValueMember
  case class Method(paramType: Type, t: Type) extends ConcreteValueMember

  case class TypeInst(con: TypeCon, args: Seq[Type]) extends Type {
    def isSubTypeOf(other: Type) = this == other
    def getMember(name: String) = con.getMember(name)
    def toSource(env: Env) = s"${env.getTypeName(con)}${(if (args.length > 0) args.mkString("[", ", ", "]") else "")}"
  }

  case class TypeParamRef(name: String) extends Type {
    def isSubTypeOf(other: Type) = this == other
    def getMember(name: String) = None
    def toSource(env: Env) = s"$name"
  }

  case object TopType extends Type {
    def isSubTypeOf(other: Type) = false
    def getMember(name: String) = None
    def toSource(env: Env) = "⊤"
  }

  case object BottomType extends Type {
    def isSubTypeOf(other: Type) = true
    def getMember(name: String) = None
    def toSource(env: Env) = "⊥"
  }

  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance

  case class TypeParam(variance: Variance = Invariant)

  case class TypeDesc(
    typeParams: Map[String, TypeParam] = Map.empty[String, TypeParam],
    parent: Option[Type],
    members: Map[String, ValueMember] = Map.empty[String, ValueMember]
  ) {
    def addTypeParam(name: String, param: TypeParam) = add(typeParams.get(name), () => this.copy(typeParams = typeParams + (name -> param)), s"Duplicate type parameter name: `$name`")

    def getParent = parent.getOrElse(TopType)

    def getMember(name: String): Option[ValueMember] = members.get(name).orElse(getParent.getMember(name))

    def addMember(name: String, member: ValueMember) = add(this.getMember(name), () => this.copy(members = members + (name -> member)), s"Already implemented: member value `$name`")

    private def add[V](v: Option[V], s: () => TypeDesc, e: String) = v.toLeft(s()).left.map(_ => Impl.error(e))
  }

  class TypeCon(desc: TypeDesc) {
    def typeParams = desc.typeParams
    def parent = desc.parent
    def getMember(name: String) = desc.getMember(name)
    def toSource(env: Env) = s"${env.getTypeName(this)}${(if (typeParams.toList.length > 0) typeParams.mkString("[", ", ", "]") else "")}"
    def inst(args: Seq[TypeInst] = Nil): Either[Errors, TypeInst] = {
      if (args.length == typeParams.toList.length)
        Right(TypeInst(this, args))
      else
        Left(Impl.error("invalid number of type arguments"))
    }
  }

  case class Env(types: HashMap[String, TypeCon], values: HashMap[String, Type]) {
    def getValue(name: String) = values.get(name).toRight(Impl.error(s"Not found: value `$name`"))
    def getTypeCon(name: String) = types.get(name).toRight(Impl.error(s"Not found: type `$name`"))
    def getNullaryTypeInst(name: String) = getTypeCon(name).flatMap(c => c.inst())
    def getTypeName(t: TypeCon): String = types.map(_.swap).get(t).getOrElse(throw new NoSuchElementException("Type name not in env: " + types.keys.toString)) // TypeCon should not exist outside envs
    def addValue(name: String, t: Type, allowShadow: Env = Env.empty) = values.get(name).flatMap { n =>
      if (allowShadow.values.get(name).isDefined) None else Some(n)
    }.toLeft(this.copy(values = values + (name -> t))).left.map(_ => Impl.error(s"Already defined: value `$name`"))
    def addType(name: String, t: TypeCon) = types.get(name).toLeft(this.copy(types = types + (name -> t))).left.map(_ => Impl.error(s"Already defined: type `$name`"))
  }

  object Env {
    def empty = Env(HashMap.empty[String, TypeCon], HashMap.empty[String, TypeInst])
  }


  // def leastUpperBound(a: TypeExpr, b: TypeExpr) = ???

  def analyze(env: Env)(valueExpr: Ast.ValueExpr): Either[Errors, Type] = Impl.walkValueExpr(env)(valueExpr)

  object Impl {
    import Ast._

    // def partitionEithers[Left : ClassTag, Right : ClassTag](in: Seq[Either[Left, Right]]): (Seq[Left], Seq[Right]) =
    //   in.partition(_.isLeft) match {
    //     case (leftList, rightList) =>
    //       (leftList.collect { case Left(l: Left) => l }, rightList.collect { case Right(r: Right) => r })
    // }

    def error(e: String) = Seq(e)

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): Either[Errors, Type] = valueExpr match {
      case UnitLiteral() => env.getNullaryTypeInst("()")
      case IntegerLiteral(_) => env.getNullaryTypeInst("Int")
      case StringLiteral(_) => env.getNullaryTypeInst("String")
      // case Tuple(es) => {
      //   val results = es.map(walkValueExpr(env))
      //
      //
      //   ???
      // }
      case Block(bcs) => walkBlock(env)(bcs)
      case NamedValue(name) => env.getValue(name)
      case ValueAs(e, asType) => walkValueExpr(env)(e) flatMap { actual =>
        walkTypeExpr(env)(asType) flatMap { t =>
          if (actual.isSubTypeOf(t))
            Right(t)
          else
            Left(error(s"Type mismatch: found ${actual.toSource(env)}, expected ${t.toSource(env)}"))
        }
      }
      // case Lambda(parameter, body) => {
      //   walkPattern(parameter, None) flatMap { paramType =>
      //     walkValueExpr(values)(body) map { resultType =>
      //       FuncType(paramType, resultType)
      //     }
      //   }
      // }
      case MemberSelection(e, memberName) => walkValueExpr(env)(e) flatMap { target =>
        target.getMember(memberName).map(_.t).toRight(error(s"type ${target.toSource(env)} does not have member $memberName"))
      }
      // case Call(callee, argument) => walkValueExpr(env)
      //   // callee must have apply member
      case ClassNew(name, members) => {
        // TODO check all members are concrete
        // TODO type params?
        env.getNullaryTypeInst(name)
      }
    }

    case class BlockState(parentEnv: Env, errors: Errors, nonUnitType: Option[Type], currentEnv: Env) {
      def result: Either[Errors, Type] = errors match {
        case Nil => nonUnitType.map(Right(_)).getOrElse(parentEnv.getNullaryTypeInst("()"))
        case errors => Left(errors)
      }
    }

    object BlockState {
      def begin(parentEnv: Env) = BlockState(parentEnv, Nil, None, parentEnv)
    }

    def walkBlock(parentEnv: Env)(bcs: Seq[BlockContent]): Either[Errors, Type] =
      walkBlockImpl(parentEnv)(bcs).result

    def walkBlockImpl(parentEnv: Env)(bcs: Seq[BlockContent]): BlockState =
      bcs.foldLeft(BlockState.begin(parentEnv)) { case (state, bc) =>
        stepBlockContent(state, bc)
      }

    def stepBlockContent(state: BlockState, bc: BlockContent): BlockState = {
      val BlockState(parentEnv, currentErrors, currentNonUnitType, currentEnv) = state
      val currentErrorsAndDiscard = currentErrors ++ (if (currentNonUnitType.isDefined) error(s"Warning: non unit value discarded in block") else Nil)

      walkBlockContent(parentEnv, currentEnv, bc) match {
        case Right((nextEnv, nonUnitType)) => BlockState(parentEnv, currentErrorsAndDiscard, nonUnitType, nextEnv)
        case Left(errors) => BlockState(parentEnv, currentErrorsAndDiscard ++ errors, None, currentEnv)
      }
    }

    def walkBlockContent(parentEnv: Env, currentEnv: Env, bc: BlockContent): Either[Errors, (Env, Option[Type])] = bc match {
      case ValueDef(target, e) =>
        walkValueExpr(currentEnv)(e) flatMap { t =>
          walkValueDef(parentEnv)(currentEnv, target, t)
        } map (newEnv => (newEnv, None))
      case ClassDef(name, typeParams, parentName, members) => {
        walkClassParent(currentEnv)(parentName) flatMap { parent =>
          walkMemberDecls(currentEnv)(TypeDesc(parent=parent), members) flatMap { updatedDesc =>
            currentEnv.addType(name, new TypeCon(updatedDesc))
          } map (newEnv => (newEnv, None))
        }
      }
      case Comment(_) => Right((currentEnv, None))
      case e: ValueExpr =>
        currentEnv.getNullaryTypeInst("()") flatMap { unit =>
          walkValueExpr(currentEnv)(e) map { t =>
            (currentEnv, Some(t).filter(_ !== unit))
          }
        }
    }

    def walkClassParent(env: Env)(parentName: Option[String]): Either[Errors, Option[Type]] = parentName match {
      case Some(parentName) => env.getNullaryTypeInst(parentName).map(Some(_))
      case None => Right(None)
    }

    def walkMemberDecls(env: Env)(desc: TypeDesc, members: Seq[MemberDecl]): Either[Errors, TypeDesc] =
      members.foldLeft((Nil: Errors, desc)) { case ((currentErrors, currentDesc), memberDecl) =>
        walkMemberDecl(env, currentDesc, memberDecl) match {
          case Right(updatedDesc) => (currentErrors, updatedDesc)
          case Left(errors) => (currentErrors ++ errors, currentDesc)
        }
      } match {
        case (Nil, updatedDesc) => Right(updatedDesc)
        case (errors, _) => Left(errors)
      }

    def walkMemberDecl(env: Env, desc: TypeDesc, memberDecl: MemberDecl): Either[Errors, TypeDesc] = memberDecl match {
        case AbstractMember(name, t) => walkTypeExpr(env)(t) flatMap { ti =>
          desc.addMember(name, AbstractValueMember(ti))
        }
        // case ClassField(name, t) =>
        //   walkTypeExpr(env)(t).flatMap(ti => desc.addMember(name, ti))
      }

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: Type): Either[Errors, Env] = target match {
      case NamePattern(name) => currentEnv.addValue(name, t, allowShadow=parentEnv)
      case _ => ???
    }

    def walkTypeExpr(env: Env)(typeExpr: Node): Either[Errors, TypeInst] = typeExpr match {
      case NamedType(name) => env.getNullaryTypeInst(name)
      case _ => ???
    }

    // case class PatternSuccess(patternType: Type, namedValues: EnvMap)

    // def walkPattern(pattern: Pattern, fromAncestor: Option[Type]): Either[Errors, PatternSuccess] = pattern match {
    //   case UnitPattern() => {
    //     val unit = UnitType()
    //
    //     fromAncestor match {
    //       case Some(at) => {
    //         if (at == unit) // == since Unit cannot be subclassed
    //           Right(PatternSuccess(at))
    //         else
    //           Left(s"Pattern does not match expected type: found ${unit}, expected $at")
    //       }
    //       case None => Right(unit)
    //     }
    //   }
    //   case TuplePattern(ps) => {
    //     val results: Seq[Either[String, TypeExpr]] = fromAncestor map { at =>
    //       at match {
    //         case TupleType(ats) if ps.length == ats.length => { // tuples cannot be subclassed
    //           (ps zip ats) map { case (p, t) => walkPattern(p, Some(t)) }
    //         }
    //         case _ => Seq(Left(s"Pattern does not match expected type: found ${ps}, expected $at"))
    //       }
    //     } getOrElse ps.map(walkPattern(_, None))
    //
    //     results collectFirst { case error @ Left(_) =>
    //       error
    //     } getOrElse {
    //       val types = results collect { case Right(typeExpr) =>
    //         typeExpr
    //       }
    //       Right(TupleType(types))
    //     }
    //   }
    //   case NamePattern(name) => fromAncestor match {
    //     case Some(t) => Right(t)
    //     case None => Left(s"Type must be specified: value `$name`")
    //   }
    //   case PatternAs(p, asType) => fromAncestor match {
    //     case Some(at) => {
    //       if (isSubtype(at, asType))
    //         walkPattern(p, Some(at))
    //       else
    //         Left(s"Type mismatch: found $asType, expected $fromAncestor")
    //     }
    //     case None => walkPattern(p, Some(asType))
    //   }
    // }

  }
}
