package clara

import scala.collection.immutable.ListMap

import ai.x.safe._

object Analyzer {

  def mapPair[A, B](f: A => B)(a: (A, A)) = (f(a._1), f(a._2))

  type Errors = Seq[Error]

  type An[A] = Either[Errors, A]

  object An {
    def apply[A](a: A) = Right(a)

    def error(pos: Pos, message: String) = Left(Impl.error(pos, message))

    def seq[A](ans: Seq[An[A]]) = {
      val (allErrors, allResults) = ans.foldLeft((Nil: Errors, Nil: Seq[A])) { case ((currentErrors, results), an) =>
        an match {
          case Right(result) => (currentErrors, results :+ result)
          case Left(errors) => (currentErrors ++ errors, results)
        }
      }

      if (allErrors.isEmpty) {
        Right(allResults)
      } else {
        Left(allErrors)
      }
    }

    def join[A, B](a: An[A], b: An[B]): An[(A, B)] = {
      //FIXME aggregate errors from both?
      a.flatMap(ar => b.map(br => (ar, br)))
    }

    def tuple[A, B](t: (An[A], An[B])) = {
      val (a, b) = t

      join(a, b)
    }
  }

  case class Namespace[I](m: ListMap[String, I]) {
    def addOrUpdate(binding: (String, I)): Namespace[I] = this.copy(m = m + binding)
    def add(binding: (String, I)): Option[Namespace[I]] = m.get(binding._1) match {
      case Some(_) => None
      case None => Some(this.addOrUpdate(binding))
    }
    def addOrShadow(binding: (String, I), allowShadow: Namespace[I]): Option[Namespace[I]] =
      if (allowShadow.get(binding._1).isDefined) {
        Some(addOrUpdate(binding))
      } else {
        add(binding)
      }
    def get(name: String): Option[I] = m.get(name)
    def getName(item: I): Option[String] = m.find { case (name, i) => i == item } map(_._1)
    def length = m.size
    def names = m.keys
    def items = m.values
    def entries = m.toList
  }

  object Namespace {
    def empty[I] = Namespace(ListMap.empty[String, I])
  }

  case class SubstMap(
    instArgs: ListMap[VarTypeCon, TypeInst],
    conArgs: ListMap[VarTypeCon, TypeCon]
  ) {
    def instArg(con: VarTypeCon): Option[TypeInst] = instArgs.get(con)
    def conArg(con: VarTypeCon): Option[TypeCon] = conArgs.get(con)
  }

  object SubstMap {
    def empty = SubstMap(ListMap(), ListMap())
    def from(typeParams: TypeParams, typeArgs: TypeArgs): Option[SubstMap] =
      if (TypeParams.validateArgs(typeParams, typeArgs)) {
        val empty = (ListMap[VarTypeCon, TypeInst](), ListMap[VarTypeCon, TypeCon]())
        val (insts, cons) = typeParams.items.zip(typeArgs).foldLeft(empty) { case ((is, cs), (param, arg)) =>
          arg match {
            case i: TypeInst => (is + ((param, i)), cs)
            case c: TypeCon => (is, cs + ((param, c)))
          }
        }

        Some(SubstMap(ListMap(insts.toSeq:_*), ListMap(cons.toSeq:_*)))
      } else None
  }


  sealed trait TypeConOrTypeInst {
    def sourceName: String
    def envName(env: Env): String
    def arity: Int
    def signature(env: Env): String
    def subst(env: Env, pos: Pos, substMap: SubstMap): An[TypeConOrTypeInst]
  }

  sealed trait TypeCon extends TypeConOrTypeInst {
    def envName(env: Env) = env.types.getName(this).getOrElse(sourceName)
    def getValueMember(env: Env, pos: Pos, name: String): Option[An[ValueMember]]
    // def validateArgs(typeArgs: TypeArgs): Boolean // TODO this is redundant to insideSubstMap
    // def valueMembers(typeArgs: TypeArgs): Namespace[ValueMember]
    def inst(env: Env, pos: Pos, typeArgs: TypeArgs): An[TypeInst] = {
      //FIXME check that all members are concrete

      // eagerly give error if type args do not match params
      insideSubstMap(env, pos, typeArgs).flatMap { _ =>
        val ti = env.getType("Function").filter(_ === this).flatMap { _ =>
          typeArgs match {
            case Seq(paramType: TypeInst, resultType: TypeInst) =>
              Some(FuncTypeInst(this, paramType, resultType))
            case _ => None
          }
        }.getOrElse(NormalTypeInst(this, typeArgs))

        An(ti)
      }
    }
    def subst(env: Env, pos: Pos, substMap: SubstMap): An[TypeCon]
    def insideSubstMap(env: Env, pos: Pos, typeArgs: TypeArgs): An[SubstMap]
  }

  type TypeParams = Namespace[VarTypeCon]

  object TypeParams {
    def empty = Namespace.empty[VarTypeCon]
    def signature(typeParams: TypeParams, env: Env) =
      if (typeParams.length > 0) typeParams.names.safeMkString("[", ", ", "]")
      else ""
    def validateArgs(typeParams: TypeParams, typeArgs: TypeArgs) = typeParams.items.map(_.arity) === typeArgs.map(_.arity)
  }

  class Uniq()

  case class ClassHeaderTypeCon(
    sourceName: String,
    typeParams: TypeParams,
    explicitParent: Option[TypeInst],
    uniq: Uniq = new Uniq()
  ) extends TypeCon {
    def signature(env: Env) = safe"${envName(env)}${TypeParams.signature(typeParams, env)}"
    def arity = typeParams.length
    def getValueMember(env: Env, pos: Pos, name: String) =
      throw new IllegalStateException("ClassHeaderTypeCon.getValueMember") // TODO or just None?
    // def validateArgs(typeArgs: TypeArgs) = TypeParams.validateArgs(typeParams, typeArgs) //TODO is this used?
    def subst(env: Env, pos: Pos, substMap: SubstMap) = An(this) //TODO substMap.classHeader(this) ?
    def insideSubstMap(env: Env, pos: Pos, typeArgs: TypeArgs) = SubstMap.from(typeParams, typeArgs).toRight(Impl.error(pos, safe"Type arguments ${TypeArgs.signature(typeArgs, env)} do not match parameters of type ${signature(env)}"))
    // def valueMembers(typeArgs: TypeArgs): Namespace[ValueMember]
  }

  case class ClassTypeCon(
    header: ClassHeaderTypeCon,
    directMembers: Members
  ) extends TypeCon {
    def sourceName = header.sourceName
    def signature(env: Env) = header.signature(env)
    def typeParams = header.typeParams
    def arity = header.arity
    def getValueMember(env: Env, pos: Pos, name: String) = {
      directMembers.values.get(name).map(An(_)).
        orElse(header.explicitParent.flatMap(_.getValueMember(env, pos, name)))
    }
    // def validateArgs(typeArgs: TypeArgs) = header.validateArgs(typeArgs)
    def subst(env: Env, pos: Pos, substMap: SubstMap) = An(this)
    def insideSubstMap(env: Env, pos: Pos, typeArgs: TypeArgs) = header.insideSubstMap(env, pos, typeArgs)
    // def valueMembers(typeArgs: TypeArgs): Namespace[ValueMember]
  }
  //   def inst(typeArgs: Seq[TypeCon], env: Env): An[TypeInst] = {
  //     if (typeArgs.length == typeParams.length)
  //       Right(NormalTypeInst(this, typeArgs))
  //     else
  //       Left(Impl.error("invalid number of type arguments for type ${this.signature(env)}"))
  //   }
  // }
  //

  case class VarTypeCon(sourceName: String, arity: Int) extends TypeCon {
    def signature(env: Env) = safe"${envName(env)}${(if (arity > 0) List.fill(arity)("_").safeMkString("[", ", ", "]") else "")}"
    def typeParams = ??? // hmm
    def getValueMember(env: Env, pos: Pos, name: String) = None
    // def validateArgs(typeArgs: TypeArgs) = typeArgs.length === arity
    def subst(env: Env, pos: Pos, substMap: SubstMap) = An(substMap.conArg(this).getOrElse(this))
    def insideSubstMap(env: Env, pos: Pos, typeArgs: TypeArgs) = if (typeArgs.length === arity) An(SubstMap.empty) else An.error(pos, safe"Type arguments ${TypeArgs.signature(typeArgs, env)} do not match parameters of type parameter ${signature(env)}")
  }
  //   def inst(typeArgs: Seq[TypeCon], env: Env) =
  //     if (typeArgs.length == typeParam.arity)
  //       Right(NormalTypeInst(this, typeArgs))
  //     else
  //       Left(Impl.error("invalid number of type arguments for type ${this.signature(env)}"))
  // }

  type TypeArgs = Seq[TypeConOrTypeInst]

  object TypeArgs {
    def signature(typeArgs: TypeArgs, env: Env) =
      if (typeArgs.length > 0) typeArgs.map(_.signature(env)).safeMkString("[", ", ", "]")
      else ""
  }

  sealed trait TypeInst extends TypeConOrTypeInst {
    def typeArgs: TypeArgs
    def isSubTypeOf(other: TypeInst): Boolean
    def arity = 0
    def getValueMember(env: Env, pos: Pos, name: String): Option[An[ValueMember]]
    def valueMember(env: Env, pos: Pos, name: String): An[ValueMember] = getValueMember(env, pos, name).getOrElse(An.error(pos, safe"type ${signature(env)} does not have member $name"))
    def valueMemberInst(env: Env, pos: Pos, name: String, methodTypeArgs: TypeArgs): An[TypeInst] =
      valueMember(env, pos, name).flatMap(_.inst(env, pos, name, methodTypeArgs))
    def subst(env: Env, pos: Pos, substMap: SubstMap): An[TypeInst]
  }

  sealed trait ConBasedTypeInst extends TypeInst {
    def con: TypeCon
    def sourceName = con.sourceName
    def envName(env: Env) = con.envName(env)
    // def substMap: SubstMap = con match {
    //   case ctCon: ClassTypeCon => Map(ctCon.header -> ctCon)
    // }
    def getValueMember(env: Env, pos: Pos, name: String) =
      con.getValueMember(env, pos, name).map(_.flatMap { m =>
        con.insideSubstMap(env, pos, typeArgs).flatMap { insideSubstMap =>
          m.subst(env, pos, insideSubstMap)
        }
      })
    def subst(env: Env, pos: Pos, substMap: SubstMap) = con match {
      // TODO better not to check VarTypeCon here but just the arity?
      case v: VarTypeCon if typeArgs == Nil => An(substMap.instArg(v).getOrElse(this))
      case _ => con.subst(env, pos, substMap) flatMap { substCon =>
        An.seq(typeArgs.map(_.subst(env, pos, substMap))) flatMap { substTypeArgs =>
          substCon.inst(env, pos, substTypeArgs)
        }
      }
    }
  }

  case class NormalTypeInst(con: TypeCon, typeArgs: TypeArgs) extends ConBasedTypeInst {
    def signature(env: Env) = safe"${envName(env)}${TypeArgs.signature(typeArgs, env)}"
    def isSubTypeOf(other: TypeInst) = this == other //FIXME
    // def valueMembers = con.valueMembers(typeArgs)
  }

  case class FuncTypeInst(con: TypeCon, paramType: TypeInst, resultType: TypeInst) extends ConBasedTypeInst {
    def typeArgs = Seq(paramType, resultType)
    def signature(env: Env) = safe"${paramType.signature(env)} => ${resultType.signature(env)}"
    def isSubTypeOf(other: TypeInst) = this == other //FIXME
  }

  sealed trait SpecialTypeInst extends TypeInst {
    def typeArgs = Nil
    def envName(env: Env) = sourceName
    def signature(env: Env) = envName(env)
    def getValueMember(env: Env, pos: Pos, name: String) = None
    def subst(env: Env, pos: Pos, substMap: SubstMap) = An(this)
  }

  case object TopType extends SpecialTypeInst {
    val sourceName = "⊤"
    def isSubTypeOf(other: TypeInst) = false
    // def valueMembers = Namespace.empty[ValueMember]
  }

  case object BottomType extends SpecialTypeInst {
    val sourceName = "⊥"
    def isSubTypeOf(other: TypeInst) = true
    // def valueMembers = Namespace.empty[ValueMember]
  }

  case class Env(types: Namespace[TypeCon], values: Namespace[TypeInst]) {
    def getValue(name: String) = values.get(name)
    def useValue(name: String, pos: Pos): An[TypeInst] = getValue(name).toRight(Impl.error(pos, safe"Not found: value `$name`"))
    def getType(name: String) = types.get(name)
    def useType(name: String, pos: Pos): An[TypeCon] = getType(name).toRight(Impl.error(pos, safe"Not found: type `$name`"))
    def addValue(binding: (String, TypeInst), pos: Pos) = addOrShadowValue(binding, Env.empty, pos)
    def addOrShadowValue(binding: (String, TypeInst), allowShadow: Env, pos: Pos): An[Env] =
      values.addOrShadow(binding, allowShadow.values).toRight(Impl.error(pos, safe"Already defined: value `${binding._1}`")).
      map(n => this.copy(values = n))
    def addType(binding: (String, TypeCon), pos: Pos) = addOrShadowType(binding, Env.empty, pos)
    def addOrShadowType(binding: (String, TypeCon), allowShadow: Env, pos: Pos): An[Env] =
      types.addOrShadow(binding, allowShadow.types).toRight(Impl.error(pos, safe"Already defined: type `${binding._1}`")).
      map(t => this.copy(types = t))
  }

  object Env {
    def empty = Env(Namespace.empty[TypeCon], Namespace.empty[TypeInst])
  }

  sealed trait ValueMember {
    def typeInst: TypeInst
    def subst(env: Env, pos: Pos, substMap: SubstMap): An[ValueMember]
    def inst(env: Env, pos: Pos, name: String, typeArgs: TypeArgs): An[TypeInst]
  }

  sealed trait PlainValueMember extends ValueMember {
    def inst(env: Env, pos: Pos, name: String, typeArgs: TypeArgs) = typeArgs match {
      case Nil => An(typeInst)
      case _ => An.error(pos, safe"Member $name does not take type arguments")
    }
  }
  sealed trait MethodMember extends ValueMember {
    def inst(env: Env, pos: Pos, name: String, typeArgs: TypeArgs) =
      SubstMap.from(typeParams, typeArgs).map { substMap =>
        typeInst.subst(env, pos, substMap)
      }.getOrElse {
        An.error(pos, safe"Type arguments ${TypeArgs.signature(typeArgs, env)} do not match parameters of method ${signature(name, env)}")
      }
    def typeParams: TypeParams
    def signature(name: String, env: Env) = safe"$name${TypeParams.signature(typeParams, env)}"
  }
  sealed trait AbstractValueMember extends ValueMember
  sealed trait ConcreteValueMember extends ValueMember
  case class PlainValueMemberDecl(typeInst: TypeInst) extends PlainValueMember with AbstractValueMember {
    def subst(env: Env, pos: Pos, substMap: SubstMap) =
      typeInst.subst(env, pos, substMap).map(substInst => this.copy(typeInst = substInst))
  }
  case class PlainValueMemberDef(typeInst: TypeInst) extends PlainValueMember with ConcreteValueMember {
    def subst(env: Env, pos: Pos, substMap: SubstMap) =
      typeInst.subst(env, pos, substMap).map(substInst => this.copy(typeInst = substInst))
  }
  case class MethodMemberDecl(typeParams: TypeParams, typeInst: TypeInst) extends MethodMember with AbstractValueMember {
    def subst(env: Env, pos: Pos, substMap: SubstMap) =
      typeInst.subst(env, pos, substMap).map(substInst => this.copy(typeInst = substInst))
  }
  case class MethodMemberDef(typeParams: TypeParams, typeInst: TypeInst) extends MethodMember with ConcreteValueMember {
    def subst(env: Env, pos: Pos, substMap: SubstMap) =
      typeInst.subst(env, pos, substMap).map(substInst => this.copy(typeInst = substInst))
  }

  case class Members(types: Namespace[TypeCon], values: Namespace[ValueMember]) {
    def addValue(binding: (String, ValueMember), pos: Pos) = addOrShadowValue(binding, Members.empty, pos)
    def addOrShadowValue(binding: (String, ValueMember), allowShadow: Members, pos: Pos): An[Members] =
      values.addOrShadow(binding, allowShadow.values).toRight(Impl.error(pos, safe"Already defined: value member `${binding._1}`")).
      map(n => this.copy(values = n))
    def addType(binding: (String, TypeCon), pos: Pos) = addOrShadowType(binding, Members.empty, pos)
    def addOrShadowType(binding: (String, TypeCon), allowShadow: Members, pos: Pos): An[Members] =
      types.addOrShadow(binding, allowShadow.types).toRight(Impl.error(pos, safe"Already defined: type member `${binding._1}`")).
      map(t => this.copy(types = t))
  }

  object Members {
    def empty = Members(Namespace.empty[TypeCon], Namespace.empty[ValueMember])
  }

  def analyze(valueExpr: Ast.ValueExpr): Either[Seq[Error], TypeInst] = Impl.walkValueExpr(Env.empty)(valueExpr)

  object Impl {
    import Ast._

    //////
    // Helpers

    def error(pos: Pos, message: String) = Seq(Error(pos, message))

    //////
    // General

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): An[TypeInst] = valueExpr match {
      case UnitLiteral(pos) => instUnit(env, pos)
      case IntegerLiteral(_, pos) => instNullary(env, pos)("Int")
      case StringLiteral(_, pos) => instNullary(env, pos)("String")
      case Tuple(es, pos) => {
        An.seq(es.map(walkValueExpr(env))).flatMap(instTuple(env, pos))
      }
      case block: Block => walkBlock(env)(block)
      case NamedValue(name, pos) => env.useValue(name, pos)
      case ValueAs(e, asType, pos) =>
        An.join(walkValueExpr(env)(e), walkTypeInstExpr(env)(asType)).flatMap(expectSubTypeOf(env, pos))
      case Lambda(parameter, body, pos) => walkLambda(env)(parameter, None, body, pos)
      case MemberSelection(e, memberName, typeArgs, memberPos, pos) =>
        An.join(walkValueExpr(env)(e), walkTypeInstExprs(env)(typeArgs)).flatMap { case (inst, args) =>
          inst.valueMemberInst(env, memberPos, memberName, args)
        }
      case Call(callee, l: Lambda, callPos) => walkCall(env, l.pos, callPos)(
        callee,
        (env, argPos, callPos) => paramType => {
          walkDelegateCallTarget(env, argPos, callPos)(paramType).
            flatMap { case FuncTypeInst(_, paramParamType, _) =>
              walkLambda(env)(l.parameter, Some(paramParamType), l.body, argPos)
            }
        }
      )
      case Call(callee, argument, pos) => walkCall(env, argument.pos, pos)(
        callee,
        (env, _, _) => _ => walkValueExpr(env)(argument)
      )
      case ClassNew(namedType, astMembers, pos) =>
        walkTypeInstExpr(env)(namedType) flatMap { ti =>
          if (astMembers.length == 0)
            An(ti)
          else
            walkMemberDecls(env)(Some(ti), astMembers, false) flatMap { members =>
              expectAllConcrete(pos)(members).map(_ => ti)
              // val name = "$AnonymousClass"
              // val anonClass = ClassTypeCon(ClassHeaderTypeCon(name, TypeParams.empty, Some(ti)), members)

              // anonClass.inst(env, pos, Nil)
            }
        }

    }

    //////
    // Block

    def walkBlock(parentEnv: Env)(block: Block): An[TypeInst] =
      block.bcs.foldLeft(WalkBlockState.begin(parentEnv, block.pos)) { case (state, bc) =>
        state.step(bc)
      }.end

    case class WalkBlockState(parentEnv: Env, blockPos: Pos, currentErrors: Errors, currentNonUnitType: Option[TypeInst], currentEnv: Env) {
      def step(bc: BlockContent): WalkBlockState = {
        val currentErrorsAndDiscard = currentErrors ++ (if (currentNonUnitType.isDefined) error(bc.pos, "Warning: non unit value discarded in block") else Nil)

        walkBlockContent(parentEnv, currentEnv, bc) match {
          case Right((nextEnv, nonUnitType)) => WalkBlockState(parentEnv, blockPos, currentErrorsAndDiscard, nonUnitType, nextEnv)
          case Left(errors) => WalkBlockState(parentEnv, blockPos, currentErrorsAndDiscard ++ errors, None, currentEnv)
        }
      }
      def end: An[TypeInst] = currentErrors match {
        case Nil => currentNonUnitType.map(Right(_)).getOrElse(instUnit(currentEnv, blockPos))
        case currentErrors => Left(currentErrors)
      }
    }

    object WalkBlockState {
      def begin(parentEnv: Env, blockPos: Pos) = WalkBlockState(parentEnv, blockPos, Nil, None, parentEnv)
    }

    def walkBlockContent(parentEnv: Env, currentEnv: Env, bc: BlockContent): An[(Env, Option[TypeInst])] = bc match {
      case ValueDef(target, e, pos) =>
        walkValueExpr(currentEnv)(e) flatMap { t =>
          walkValueDef(parentEnv)(currentEnv, target, t)
        } map (newEnv => (newEnv, None))
      case classDef: ClassDef => walkClassDef(currentEnv)(classDef) map (newEnv => (newEnv, None))
      case Comment(_, pos) => Right((currentEnv, None))
      case e: ValueExpr => An.join(instUnit(currentEnv, e.pos), walkValueExpr(currentEnv)(e)).map { case (unit, t) => (currentEnv, Some(t).filter(_ !== unit))
      }
    }

    //////
    // Lambda

    def walkLambda(env: Env)(parameter: Pattern, expectedParameterType: Option[TypeInst], body: ValueExpr, pos: Pos): An[TypeInst] =
      walkPattern(env)(parameter, expectedParameterType).flatMap {
        case Some(PatternResult(typeInst, names)) =>
          names.entries.foldLeft(An(env): An[Env]) { case (currentEnv, binding) =>
            currentEnv.flatMap(e => e.addOrShadowValue(binding, env, pos))
          }.flatMap { bodyEnv =>
            walkValueExpr(bodyEnv)(body).flatMap { bodyType =>
              instFunction(env, pos)((typeInst, bodyType))
            }
          }
        case None => An.error(pos, "Must specify parameter type in lambda")
      }

    //////
    // Call

    def walkCall(env: Env, argPos: Pos, callPos: Pos)(
      callee: ValueExpr,
      inferArgumentType: (Env, Pos, Pos) => (TypeInst) => An[TypeInst]
    ): An[TypeInst] = {
      walkValueExpr(env)(callee).flatMap { originalCalleeType =>
        walkDelegateCallTarget(env, argPos, callPos)(originalCalleeType).
          flatMap { case FuncTypeInst(con, paramType, resultType) =>
            inferArgumentType(env, argPos, callPos)(paramType).flatMap { argType =>
              if (argType.isSubTypeOf(paramType)) {
                An(resultType)
              } else {
                An.error(argPos, safe"Type mismatch in call: argument was found to be ${argType.signature(env)}, expected parameter type is ${paramType.signature(env)}")
              }
            }
          }
      }
    }

    def walkDelegateCallTarget(env: Env, argPos: Pos, callPos: Pos)(calleeType: TypeInst): An[FuncTypeInst] = calleeType match {
      case func: FuncTypeInst => An(func)
      case _ => calleeType.getValueMember(env, callPos, "apply").map { applyMember =>
        applyMember.
          flatMap(_.inst(env, callPos, "apply", Nil)).
          flatMap(walkDelegateCallTarget(env, argPos, callPos)(_))
      } getOrElse {
        An.error(argPos, safe"type `${calleeType.signature(env)}` cannot be called (does not have member `apply`)")
      }
    }

    //////
    // ClassDef

    def walkClassDef(env: Env)(classDef: ClassDef): An[Env] = {
      val ClassDef(name, astTypeParams, parent, astMembers, pos) = classDef

      walkTypeParams(env)(astTypeParams) flatMap { case (insideEnv, typeParams) =>
        walkClassParent(insideEnv)(parent) flatMap { parentInst =>
          val selfHead = ClassHeaderTypeCon(name, typeParams, parentInst)

          insideEnv.addType((name, selfHead), pos) flatMap { bodyEnv =>
            walkMemberDecls(bodyEnv)(parentInst, astMembers, true) flatMap { members =>
              env.addType((name, ClassTypeCon(selfHead, members)), pos)
            }
          }
        }
      }
    }

    def walkTypeParams(env: Env)(typeParams: Seq[Ast.TypeParam]): An[(Env, TypeParams)] =
      typeParams.foldLeft(Nil: Errors, (env, TypeParams.empty)) { case ((currentErrors, (currentEnv, currentTypeParams)), typeParam) => {
        val Ast.TypeParam(variance, name, arity, pos) = typeParam

        val varTypeCon = VarTypeCon(name, arity)
        currentEnv.addType((name, varTypeCon), pos) flatMap { updatedEnv =>
          currentTypeParams.add((name, varTypeCon)).
            toRight(error(pos, safe"Duplicate type parameter name $name")).
            map { updatedTypeParams =>
              (updatedEnv, updatedTypeParams)
            }
        } match {
          case Right(updated) => (currentErrors, updated)
          case Left(errors) => (currentErrors ++ errors, (currentEnv, currentTypeParams))
        }
      }} match {
        case (Nil, updated) => Right(updated)
        case (errors, _) => Left(errors)
      }

    // FIXME impl seems reusable with small modifications
    def walkClassParent(env: Env)(parent: Option[NamedType]): An[Option[TypeInst]] = parent match {
      case Some(namedType) => walkTypeInstExpr(env)(namedType).map(Some(_))
      case None => An(None)
    }

    def walkMemberDecls(env: Env)(parentInst: Option[TypeInst], members: Seq[MemberDecl], allowNewMembers: Boolean): An[Members] =
      members.foldLeft(WalkMemberDeclsState.begin(env, parentInst)) { case (state, memberDecl) =>
        state.step(memberDecl)
      }.end

    case class WalkMemberDeclsState(env: Env, parentInst: Option[TypeInst], currentErrors: Errors, currentDirectMembers: Members) {
      def noValueInParent(name: String, pos: Pos): An[Unit] = parentInst.flatMap(_.getValueMember(env, pos, name)) match {
          case Some(_) => An.error(pos, safe"Member value `$name` already defined in parent")
          case None => An(())
        }
      // def currentValueMember(name: String): Option[ValueMember] = currentDirectMembers.values.get(name).orElse(
      //   parentInst.flatMap(_.valueMember(name))
      // )
      def step(memberDecl: MemberDecl): WalkMemberDeclsState =
        this.walkMemberDecl(memberDecl) match {
          case Right(updatedDirectMembers) => this.copy(currentDirectMembers=updatedDirectMembers)
          case Left(errors) => this.copy(currentErrors=(currentErrors ++ errors))
        }
      def walkMemberDecl(memberDecl: MemberDecl): An[Members] = memberDecl match {
        case ValueDecl(name, t, pos) => noValueInParent(name, pos).flatMap { _ =>
          walkTypeInstExpr(env)(t) flatMap { ti =>
            currentDirectMembers.addValue((name, PlainValueMemberDecl(ti)), pos)
          }
        }
        case MethodDecl(name, typeParams, t, pos) => noValueInParent(name, pos).flatMap { _ =>
          walkTypeParams(env)(typeParams).flatMap { case (insideEnv, paramInsts) =>
            walkTypeInstExpr(insideEnv)(t) flatMap { ti =>
              currentDirectMembers.addValue((name, MethodMemberDecl(paramInsts, ti)), pos)
            }
          }
        }
        case ValueDef(NamePattern(name, namePos), e, defPos) => parentInst.flatMap(_.getValueMember(env, defPos, name)) match {
            case Some(existing) => existing.flatMap {
              case _: ConcreteValueMember => An.error(defPos, safe"Member already defined: `$name`")
              case _: MethodMemberDecl =>
                An.error(defPos, safe"Member `$name` is declared to be a method.")
              case decl: PlainValueMemberDecl =>
                walkValueExpr(env)(e).flatMap { actualInst =>
                  // TODO: maybe less generic error message
                  expectSubTypeOf(env, defPos)((actualInst, decl.typeInst)).flatMap { _ =>
                    currentDirectMembers.addValue((name, PlainValueMemberDef(actualInst)), defPos)
                  }
                }
            }
            case None =>
              walkValueExpr(env)(e).flatMap { ti =>
                currentDirectMembers.addValue((name, PlainValueMemberDef(ti)), defPos)
              }
          }
        case ValueDef(complexPattern, _, _) => An.error(complexPattern.pos, "Error: Complex patterns are not allowed in class member value.")
        case MethodDef(name, typeParams, body, pos) =>
          parentInst.flatMap(_.getValueMember(env, pos, name)) match {
            case Some(existing) => existing.flatMap {
              case _: ConcreteValueMember => An.error(pos, safe"Member already defined: `$name`")
              case _: PlainValueMemberDecl =>
                An.error(pos, safe"Member `$name` is declared to be a plain value.")
              case decl: MethodMemberDecl =>
                walkTypeParams(env)(typeParams).flatMap { case (insideEnv, actualTypeParams) =>
                  if (actualTypeParams.items.map(_.arity) === decl.typeParams.items.map(_.arity)) {
                    walkValueExpr(insideEnv)(body).flatMap { actualInst =>
                      expectSubTypeOf(env, pos)((actualInst, decl.typeInst)).flatMap { _ =>
                        currentDirectMembers.addValue((name, MethodMemberDef(actualTypeParams, actualInst)), pos)
                      }
                    }
                  } else {
                    An.error(pos, safe"Type parameters ${TypeParams.signature(actualTypeParams, env)} do not match declaration ${TypeParams.signature(decl.typeParams, env)}")
                  }
                }
            }
            case None =>
              walkTypeParams(env)(typeParams).flatMap { case (insideEnv, params) =>
                walkValueExpr(insideEnv)(body).flatMap { ti =>
                  currentDirectMembers.addValue((name, MethodMemberDef(params, ti)), pos)
                }
              }
          }
      }
      def end = currentErrors match {
        case Nil => Right(currentDirectMembers)
        case _ => Left(currentErrors)
      }
    }

    object WalkMemberDeclsState {
      def begin(env: Env, parentInst: Option[TypeInst]) = WalkMemberDeclsState(env, parentInst, Nil, Members.empty)
    }

    def expectAllConcrete(pos: Pos)(members: Members): An[Unit] = {
      // TODO check type members also if we add them
      members.values.entries.filter {
        case (_, avm: AbstractValueMember) => true
        case (_, cvm: ConcreteValueMember) => false
      } match {
        case Nil => An(())
        case abstracts => Left(abstracts.map { case (name, avm) =>
          Error(pos, safe"Value member `$name` needs to be defined")
        })
      }
    }

    //////
    // --- TODO sort these:

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: TypeInst): An[Env] = target match {
      case NamePattern(name, pos) => currentEnv.addOrShadowValue((name, t), parentEnv, pos)
      case _ => ???
    }

    def walkTypeExpr(env: Env)(typeExpr: TypeExpr): An[TypeConOrTypeInst] = typeExpr match {
      case UnitType(pos) => instUnit(env, pos)
      case NamedType(name, typeArgs, pos) => walkNamedTypeExpr(env, pos)(name, typeArgs)
      case FuncType(parameterExpr, resultExpr, pos) =>
        An.tuple(mapPair(walkTypeInstExpr(env))((parameterExpr, resultExpr))).
          flatMap(instFunction(env, pos))
      case TupleType(typeExprs, pos) =>
        walkTypeInstExprs(env)(typeExprs).flatMap(instTuple(env, pos))
    }

    def expectTypeInst(env: Env, pos: Pos)(conOrInst: TypeConOrTypeInst): An[TypeInst] = conOrInst match {
      case con: TypeCon => An.error(pos, safe"Expected instantiated type but got type constructor ${con.signature(env)}")
      case inst: TypeInst => An(inst)
    }

    def walkTypeInstExpr(env: Env)(typeExpr: TypeExpr): An[TypeInst] =
      walkTypeExpr(env)(typeExpr).flatMap(expectTypeInst(env, typeExpr.pos))

    def walkTypeInstExprs(env: Env)(typeExprs: Seq[TypeExpr]): An[Seq[TypeInst]] =
      An.seq(typeExprs.map(walkTypeInstExpr(env)))

    def walkNamedTypeExpr(env: Env, pos: Pos)(name: String, typeArgs: Seq[TypeExpr]): An[TypeConOrTypeInst] =
      env.useType(name, pos).flatMap { con =>
        if (con.arity > 0 && typeArgs.length == 0) {
          An(con)
        } else {
          An.seq(typeArgs.map(walkTypeExpr(env))) flatMap { args =>
            con.inst(env, pos, args)
          }
        }
      }

    def instNullary(env: Env, pos: Pos)(name: String): An[TypeInst] = env.useType(name, pos).flatMap(_.inst(env, pos, Nil))

    def instUnit(env: Env, pos: Pos): An[TypeInst] = instNullary(env, pos)("()")

    def instTuple(env: Env, pos: Pos)(types: Seq[TypeInst]): An[TypeInst] =
      if (types.length == 2) {
        env.useType("Tuple", pos).flatMap { con =>
          con.inst(env, pos, types)
        }
      } else {
        An.error(pos, "Only 2-tuples are supported")
      }

    def instFunction(env: Env, pos: Pos)(tt: (TypeInst, TypeInst)): An[TypeInst] =
      env.useType("Function", pos).flatMap { con =>
        con.inst(env, pos, Seq(tt._1, tt._2))
      }

    def expectSubTypeOf(env: Env, pos: Pos)(tt: (TypeInst, TypeInst)): An[TypeInst] = {
      val (sub, sup) = tt

      if (sub.isSubTypeOf(sup))
        An(sub)
      else
        An.error(pos, safe"Type mismatch: found ${sub.signature(env)}, expected ${sup.signature(env)}")
    }

    //////
    // Pattern

    case class PatternResult(typeInst: TypeInst, names: Namespace[TypeInst])

    def walkPattern(env: Env)(pattern: Pattern, expectedType: Option[TypeInst]): An[Option[PatternResult]] = pattern match {
      case UnitPattern(pos) => instUnit(env, pos).map(u => Some(PatternResult(u, Namespace.empty)))
      // case TuplePattern(ps) => env.useType("Tuple").flatMap { tupleCon =>
      //   actual match {
      //     case Some(NormalTypeInst(con, args)) if con === tupleCon && ps.length == 2 && args.length == 2 =>
      //       An.seq((ps zip args).map { case (p, arg) =>
      //         expectTypeInst(env)(arg) flatMap { argTi =>
      //           walkPattern(env)(p, Some(argTi))
      //         }
      //       }).map { results =>
      //         val updatedNames = results.map(_.names).foldLeft(Namespace.empty) { case (acc, ns) =>
      //           acc.addAll(ns.entries)
      //         }
      //
      //         PatternResult(tuple, updatedNames)
      //       }
      //     case _ => An.error(safe"Expected (?, ?) but got $actual")
      //   }
      // }
      case NamePattern(name, pos) => expectedType.map { t =>
        Namespace.empty.add((name, t)).map { names =>
          An(Some(PatternResult(t, names)))
        }.getOrElse(An.error(pos, safe"Duplicate value name `$name` in pattern"))
      }.getOrElse(An(None))
      case PatternAs(p, asType, pos) => walkTypeInstExpr(env)(asType).flatMap { asTypeInst =>
        walkPattern(env)(p, Some(asTypeInst)).flatMap {
          case Some(PatternResult(typeInst, names)) =>
            expectSubTypeOf(env, pos)((typeInst, asTypeInst)).map(t => Some(PatternResult(t, names)))
          case None => An(Some(PatternResult(asTypeInst, Namespace.empty)))
        }
      }
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
    //           Left(safe"Pattern does not match expected type: found ${unit}, expected $at")
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
    //         case _ => Seq(Left(safe"Pattern does not match expected type: found ${ps}, expected $at"))
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
    //     case None => Left(safe"Type must be specified: value `$name`")
    //   }
    //   case PatternAs(p, asType) => fromAncestor match {
    //     case Some(at) => {
    //       if (isSubtype(at, asType))
    //         walkPattern(p, Some(at))
    //       else
    //         Left(safe"Type mismatch: found $asType, expected $fromAncestor")
    //     }
    //     case None => walkPattern(p, Some(asType))
    //   }
    // }

  }
}
