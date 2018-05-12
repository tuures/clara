package clara

import collection.immutable.HashMap

import ai.x.safe._

object Analyzer {
  //TODO move these helpers
  implicit class SafeMap[K, V](map: Map[K, V]) {
    def add(kv: (K, V)): Option[Map[K, V]] = map.get(kv._1) match {
      case Some(_) => None
      case None => Some(map + kv)
    }
  }

  type Errors = Seq[String]

  type An[A] = Either[Errors, A]

  case class Env(typesByName: HashMap[String, TypeCon], valuesByName: HashMap[String, TypeInst]) {
    def getValue(name: String) = valuesByName.get(name).toRight(Impl.error(s"Not found: value `$name`"))
    def getTypeCon(name: String) = typesByName.get(name).toRight(Impl.error(s"Not found: type `$name`"))
    // def getTypeInst(name: String)
    def getNullaryTypeInst(name: String) = getTypeCon(name).flatMap(c => c.inst(Nil, this))
    def getTypeName(t: TypeCon): String = typesByName.map(_.swap).get(t).getOrElse(throw new NoSuchElementException("Type name not in env: " + typesByName.keys.toString)) // TypeCon should not exist outside envs
    def addValue(name: String, t: TypeInst, allowShadow: Env = Env.empty): An[Env] =
      valuesByName.get(name).
        filterNot(_ => allowShadow.valuesByName.get(name).isDefined).
        toLeft(this.copy(valuesByName = valuesByName + (name -> t))).
        left.map(_ => Impl.error(s"Already defined: value `$name`"))
    def addType(name: String, t: TypeCon, allowShadow: Env = Env.empty): An[Env] =
      typesByName.get(name).
        filterNot(_ => allowShadow.valuesByName.get(name).isDefined).
        toLeft(this.copy(typesByName = typesByName + (name -> t))).
        left.map(_ => Impl.error(s"Already defined: type `$name`"))
  }

  object Env {
    def empty = Env(HashMap.empty[String, TypeCon], HashMap.empty[String, TypeInst])
  }

  sealed trait TypeCon {
    def sourceName: String
    def valueMembers(name: String): Option[ValueMember]
    def inst(typeArgs: Seq[TypeCon], env: Env): An[TypeInst]
  }

  class NormalTypeCon(
    val sourceName: String,
    val typeParams: Seq[Ast.TypeParam],
    val explicitParent: Option[TypeCon],
    directValueMembers: Map[String, ValueMember]
  ) extends TypeCon {
    def parent = explicitParent.getOrElse(TopType)
    def valueMembers(name: String) = directValueMembers.get(name).orElse(parent.valueMembers(name))
    def toSource(env: Env) = s"${env.getTypeName(this)}${(if (typeParams.length > 0) typeParams.mkString("[", ", ", "]") else "")}"
    def inst(typeArgs: Seq[TypeCon], env: Env): An[TypeInst] = {
      if (typeArgs.length == typeParams.length)
        Right(NormalTypeInst(this, typeArgs))
      else
        Left(Impl.error("invalid number of type arguments for type ${this.toSource(env)}"))
    }
  }

  case class ApplyTypeCon(typeCon: TypeCon, typeArgs: Seq[TypeCon])

  type Variance = Ast.Variance

  type TypeConParam = Ast.TypeParam

  sealed trait ValueMember {
    def t: TypeInst
  }
  // case class AbstractValueMember(t: TypeInst) extends ValueMember
  // sealed trait ConcreteValueMember extends ValueMember
  case class PlainValueMember(t: TypeInst) extends ValueMember
  // case class Method(paramType: TypeInst, t: TypeInst) extends ConcreteValueMember

  case class TypeParamTypeCon(typeParam: Ast.TypeParam) extends TypeCon {
    def sourceName = typeParam.name
    def valueMembers(name: String) = None
    def inst(typeArgs: Seq[TypeCon], env: Env) =
      if (typeArgs.length == typeParam.arity)
        Right(NormalTypeInst(this, typeArgs))
      else
        Left(Impl.error("invalid number of type arguments for type ${this.toSource(env)}"))
  }

  sealed trait TypeInst {
    def sourceName: String
    def isSubTypeOf(other: TypeInst): Boolean
    def valueMembers(name: String): Option[ValueMember]
    def toSource(env: Env): String
  }

  case class NormalTypeInst(con: TypeCon, typeArgs: Seq[TypeCon]) extends TypeInst {
    def sourceName = con.sourceName
    def isSubTypeOf(other: TypeInst) = this == other
    def valueMembers(name: String) = ??? //con.valueMember(name)
    def toSource(env: Env) = s"${env.getTypeName(con)}${(if (typeArgs.length > 0) typeArgs.mkString("[", ", ", "]") else "")}"
  }

  case object TopType extends TypeInst {
    val sourceName = "⊤"
    def isSubTypeOf(other: TypeInst) = false
    def valueMembers(name: String) = None
    def toSource(env: Env) = sourceName
  }

  case object BottomType extends TypeInst {
    val sourceName = "⊥"
    def isSubTypeOf(other: TypeInst) = true
    def valueMembers(name: String) = None
    def toSource(env: Env) = sourceName
  }

  // def leastUpperBound(a: TypeExpr, b: TypeExpr) = ???

  def analyze(valueExpr: Ast.ValueExpr): An[TypeInst] = Impl.walkValueExpr(Env.empty)(valueExpr)

  object Impl {
    import Ast._

    // def partitionEithers[Left : ClassTag, Right : ClassTag](in: Seq[Either[Left, Right]]): (Seq[Left], Seq[Right]) =
    //   in.partition(_.isLeft) match {
    //     case (leftList, rightList) =>
    //       (leftList.collect { case Left(l: Left) => l }, rightList.collect { case Right(r: Right) => r })
    // }

    def error(e: String) = Seq(e)

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): An[TypeInst] = valueExpr match {
      case UnitLiteral() => env.getNullaryTypeInst("()")
      case IntegerLiteral(_) => env.getNullaryTypeInst("Int")
      case StringLiteral(_) => env.getNullaryTypeInst("String")
      // case Tuple(es) => {
      //   val results = es.map(walkValueExpr(env))
      //
      //
      //   ???
      // }
      case block: Block => walkBlock(env)(block)
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
      case MemberSelection(e, memberName, typeArgs) => walkValueExpr(env)(e) flatMap { target =>
        target.valueMembers(memberName).map(_.t).toRight(error(s"type ${target.toSource(env)} does not have member $memberName"))
      }
      // case Call(callee, argument) => walkValueExpr(env)
      //   // callee must have apply member
      case ClassNew(NamedType(name, typeArgs), members) => {
        // TODO implement given members, check all members are concrete
        env.getTypeCon(name).inst(typeArgs)
      }
    }

    //////
    // Block

    def walkBlock(parentEnv: Env)(block: Block): An[TypeInst] =
      block.bcs.foldLeft(WalkBlockState.begin(parentEnv)) { case (state, bc) =>
        state.step(bc)
      }.end

    case class WalkBlockState(parentEnv: Env, currentErrors: Errors, currentNonUnitType: Option[TypeInst], currentEnv: Env) {
      def step(bc: BlockContent): WalkBlockState = {
        val currentErrorsAndDiscard = currentErrors ++ (if (currentNonUnitType.isDefined) error(s"Warning: non unit value discarded in block") else Nil)

        walkBlockContent(parentEnv, currentEnv, bc) match {
          case Right((nextEnv, nonUnitType)) => WalkBlockState(parentEnv, currentErrorsAndDiscard, nonUnitType, nextEnv)
          case Left(errors) => WalkBlockState(parentEnv, currentErrorsAndDiscard ++ errors, None, currentEnv)
        }
      }
      def end: An[TypeInst] = currentErrors match {
        case Nil => currentNonUnitType.map(Right(_)).getOrElse(parentEnv.getNullaryTypeInst("()"))
        case currentErrors => Left(currentErrors)
      }
    }

    object WalkBlockState {
      def begin(parentEnv: Env) = WalkBlockState(parentEnv, Nil, None, parentEnv)
    }

    def walkBlockContent(parentEnv: Env, currentEnv: Env, bc: BlockContent): An[(Env, Option[TypeInst])] = bc match {
      case ValueDef(target, e) =>
        walkValueExpr(currentEnv)(e) flatMap { t =>
          walkValueDef(parentEnv)(currentEnv, target, t)
        } map (newEnv => (newEnv, None))
      case classDef: ClassDef => walkClassDef(currentEnv)(classDef) map (newEnv => (newEnv, None))
      case Comment(_) => Right((currentEnv, None))
      case e: ValueExpr =>
        currentEnv.getNullaryTypeInst("()") flatMap { unit =>
          walkValueExpr(currentEnv)(e) map { t =>
            (currentEnv, Some(t).filter(_ !== unit))
          }
        }
    }

    //////
    // ClassDef

    def walkClassDef(env: Env)(classDef: ClassDef): An[Env] = {
      val ClassDef(name, typeParams, parent, members) = classDef

      walkClassTypeParams(env)(typeParams) flatMap { insideEnv =>
        walkClassParent(insideEnv)(parent) flatMap { parentInst =>
          walkMemberDecls(insideEnv)(parentInst, members) flatMap { valueMembers =>
            env.addType(name, new NormalTypeCon(name, typeParams, parentInst, valueMembers))
          }
        }
      }
    }

    // def foldEither

    def walkClassTypeParams(env: Env)(typeParams: Seq[TypeParam]): An[Env] =
      typeParams.foldLeft((Nil: Errors, env)) { case ((currentErrors, currentEnv), typeParam) =>
        currentEnv.addType(typeParam.name, new TypeParamTypeCon(typeParam)) match {
          case Right(updatedEnv) => (currentErrors, updatedEnv)
          case Left(errors) => (currentErrors ++ errors, currentEnv)
        }
      } match {
        case (Nil, updatedEnv) => Right(updatedEnv)
        case (errors, _) => Left(errors)
      }

    def walkClassParent(env: Env)(parent: Option[NamedType]): An[Option[ApplyTypeCon]] = parent map {
      case Some(N) => env.getNullaryTypeInst(parentName).map(Some(_))
      case None => Right(None)
    }

    def walkMemberDecls(env: Env)(parentInst: Option[TypeInst], members: Seq[MemberDecl]): An[Map[String, ValueMember]] =
      members.foldLeft(WalkMemberDeclsState.begin(env, parentInst)) { case (state, memberDecl) =>
        state.step(memberDecl)
      }.end

    case class WalkMemberDeclsState(env: Env, parentInst: Option[TypeInst], currentErrors: Errors, currentDirectMembers: Env) {
      def currentValueMembers(name: String): Option[ValueMember] = currentDirectMembers.getValue(name).orElse(
        parentInst.flatMap(_.valueMembers(name))
      )
      def step(memberDecl: MemberDecl): WalkMemberDeclsState =
        this.walkMemberDecl(memberDecl) match {
          case Right(updatedDirectMembers) => this.copy(currentDirectMembers=updatedDirectMembers)
          case Left(errors) => this.copy(currentErrors=(currentErrors ++ errors))
        }
      def walkMemberDecl(memberDecl: MemberDecl): An[Env] = memberDecl match {
        case ValueDecl(name, t) => this.currentValueMembers(name).
          map(_ => Left(error(s"Member already declared: `$name`"))).
          getOrElse {
            walkTypeExpr(env)(t) flatMap { ti =>
              currentDirectMembers.addValue(name, PlainValueMember(ti))
            }
          }
        case ValueDef(NamePattern(name), t) => this.currentValueMembers(name) match {
          case None | Some(decl: MemberDecl) =>
            walkTypeExpr(env)(t) flatMap { ti =>
              currentDirectMembers.addValue(name, PlainValueMember(ti))
            }
          case Some(def: MemberDef) => Left(error(s"Member already defined: `$name`")
        }
        case complex: ValueDef => Left(error("Error: Complex pattern not allowed in class member value declaration."))
        // case MethodDecl(name, t) => walkTypeExpr(env)(t) flatMap { ti =>
        //   desc.addMember(name, AbstractValueMember(ti))
        // }
        // case MethodDef(name, typeParams, body) => walkValueExpr(env)(body) flatMap { ti =>
        //   desc.addMember(name, Method(ti))
        // }
      }
      def end = currentErrors match {
        case Nil => Right(currentDirectValueMembers)
        case _ => Left(_)
      }
    }

    object WalkMemberDeclsState {
      def begin(env: Env, parentInst: Option[TypeInst]) = WalkMemberDeclsState(env, parentInst, Nil, Map.empty[String, ValueMember])
    }

    //////
    // ---

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: TypeInst): An[Env] = target match {
      case NamePattern(name) => currentEnv.addValue(name, t, allowShadow=parentEnv)
      case _ => ???
    }

    def walkTypeExpr(env: Env)(typeExpr: Node): An[TypeInst] = typeExpr match {
      case NamedType(name, typeArgs) => env.getNullaryTypeInst(name)
      case _ => ???
    }

    // case class PatternSuccess(patternType: TypeInst, namedValues: EnvMap)

    // def walkPattern(pattern: Pattern, fromAncestor: Option[TypeInst]): An[PatternSuccess] = pattern match {
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
