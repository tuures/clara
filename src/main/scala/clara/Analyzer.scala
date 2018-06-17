package clara

import scala.collection.immutable.ListMap

import ai.x.safe._

object Analyzer {

  def mapPair[A, B](f: A => B)(a: (A, A)) = (f(a._1), f(a._2))

  // TODO make error case class instead of plain string
  type Errors = Seq[String]

  type An[A] = Either[Errors, A]

  object An {
    def apply[A](a: A) = Right(a)

    def error(e: String) = Left(Impl.error(e))

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
    def from(typeParams: TypeParams, typeArgs: TypeArgs) =
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
    def subst(env: Env, substMap: SubstMap): An[TypeConOrTypeInst]
  }

  sealed trait TypeCon extends TypeConOrTypeInst {
    def envName(env: Env) = env.types.getName(this).getOrElse(sourceName)
    def getValueMember(name: String): Option[ValueMember]
    def validateArgs(typeArgs: TypeArgs): Boolean
    // def valueMembers(typeArgs: TypeArgs): Namespace[ValueMember]
    def inst(env: Env, typeArgs: TypeArgs): An[TypeInst] = {
      //FIXME check that all members are concrete
      if (!validateArgs(typeArgs)) {
        An.error(s"Type arguments ${TypeArgs.signature(typeArgs, env)} do not match parameters of type ${this.signature(env)}")
      } else {
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
    def subst(env: Env, substMap: SubstMap): An[TypeCon]
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
    def signature(env: Env) = s"${envName(env)}${TypeParams.signature(typeParams, env)}"
    def arity = typeParams.length
    def getValueMember(name: String) = throw new IllegalStateException("ClassHeaderTypeCon.valueMember") // TODO or just None?
    def validateArgs(typeArgs: TypeArgs) = TypeParams.validateArgs(typeParams, typeArgs)
    def subst(env: Env, substMap: SubstMap) = An(this) //TODO substMap.classHeader(this)
    // def valueMembers(typeArgs: TypeArgs): Namespace[ValueMember]
  }

  case class ClassTypeCon(
    header: ClassHeaderTypeCon,
    directMembers: Members
  ) extends TypeCon {
    def sourceName = header.sourceName
    def signature(env: Env) = header.signature(env)
    def arity = header.arity
    def getValueMember(name: String) = {
      directMembers.values.get(name).
        orElse(header.explicitParent.flatMap(_.getValueMember(name)))
        //TODO subst in parent
    }
    def validateArgs(typeArgs: TypeArgs) = header.validateArgs(typeArgs)
    def subst(env: Env, substMap: SubstMap) = An(this)
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
    def signature(env: Env) = s"${envName(env)}${(if (arity > 0) List.fill(arity)("_").safeMkString("[", ", ", "]") else "")}"
    def getValueMember(name: String) = None
    def validateArgs(typeArgs: TypeArgs) = typeArgs.length === arity
    def subst(env: Env, substMap: SubstMap) = An(substMap.conArg(this).getOrElse(this))
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
    def getValueMember(name: String): Option[ValueMember]
    def valueMember(env: Env, name: String): An[ValueMember] = getValueMember(name).toRight(Impl.error(s"type ${signature(env)} does not have member $name"))
    def valueMemberInst(env: Env, name: String, methodTypeArgs: TypeArgs): An[TypeInst] =
      valueMember(env, name).flatMap(_.inst(env, name, methodTypeArgs))
    def subst(env: Env, substMap: SubstMap): An[TypeInst]
  }

  sealed trait ConBasedTypeInst extends TypeInst {
    def con: TypeCon
    def sourceName = con.sourceName
    def envName(env: Env) = con.envName(env)
    // def substMap: SubstMap = con match {
    //   case ctCon: ClassTypeCon => Map(ctCon.header -> ctCon)
    // }
    def getValueMember(name: String) = con.getValueMember(name)
    def subst(env: Env, substMap: SubstMap) = con match {
      case v: VarTypeCon if typeArgs == Nil => An(substMap.instArg(v).getOrElse(this))
      case _ => con.subst(env, substMap) flatMap { substCon =>
        An.seq(typeArgs.map(_.subst(env, substMap))) flatMap { substTypeArgs =>
          substCon.inst(env, substTypeArgs)
        }
      }
    }
  }

  case class NormalTypeInst(con: TypeCon, typeArgs: TypeArgs) extends ConBasedTypeInst {
    def signature(env: Env) = s"${envName(env)}${TypeArgs.signature(typeArgs, env)}"
    def isSubTypeOf(other: TypeInst) = this == other //FIXME
    // def valueMembers = con.valueMembers(typeArgs)
  }

  case class FuncTypeInst(con: TypeCon, paramType: TypeInst, resultType: TypeInst) extends ConBasedTypeInst {
    def typeArgs = Seq(paramType, resultType)
    def signature(env: Env) = s"${paramType.signature(env)} => ${resultType.signature(env)}"
    def isSubTypeOf(other: TypeInst) = this == other //FIXME
  }

  sealed trait SpecialTypeInst extends TypeInst {
    def typeArgs = Nil
    def envName(env: Env) = sourceName
    def signature(env: Env) = envName(env)
    def getValueMember(name: String) = None
    def subst(env: Env, substMap: SubstMap) = An(this)
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
    def useValue(name: String): An[TypeInst] = getValue(name).toRight(Impl.error(s"Not found: value `$name`"))
    def getType(name: String) = types.get(name)
    def useType(name: String): An[TypeCon] = getType(name).toRight(Impl.error(s"Not found: type `$name`"))
    def addValue(binding: (String, TypeInst)) = addOrShadowValue(binding, Env.empty)
    def addOrShadowValue(binding: (String, TypeInst), allowShadow: Env): An[Env] =
      values.addOrShadow(binding, allowShadow.values).toRight(Impl.error(s"Already defined: value `${binding._1}`")).
      map(n => this.copy(values = n))
    def addType(binding: (String, TypeCon)) = addOrShadowType(binding, Env.empty)
    def addOrShadowType(binding: (String, TypeCon), allowShadow: Env): An[Env] =
      types.addOrShadow(binding, allowShadow.types).toRight(Impl.error(s"Already defined: type `${binding._1}`")).
      map(t => this.copy(types = t))
  }

  object Env {
    def empty = Env(Namespace.empty[TypeCon], Namespace.empty[TypeInst])
  }

  sealed trait ValueMember {
    def typeInst: TypeInst
    def inst(env: Env, name: String, typeArgs: TypeArgs): An[TypeInst]
  }

  sealed trait PlainValueMember extends ValueMember {
    def inst(env: Env, name: String, typeArgs: TypeArgs) = typeArgs match {
      case Nil => An(typeInst)
      case _ => An.error(s"Member $name does not take type arguments")
    }
  }
  sealed trait MethodMember extends ValueMember {
    def inst(env: Env, name: String, typeArgs: TypeArgs) =
      SubstMap.from(typeParams, typeArgs).map { substMap =>
        typeInst.subst(env, substMap)
      }.getOrElse {
        An.error(s"Type arguments ${TypeArgs.signature(typeArgs, env)} do not match parameters of method ${signature(name, env)}")
      }
    def typeParams: TypeParams
    def signature(name: String, env: Env) = s"$name${TypeParams.signature(typeParams, env)}"
  }
  sealed trait AbstractValueMember extends ValueMember
  sealed trait ConcreteValueMember extends ValueMember
  case class PlainValueMemberDecl(typeInst: TypeInst) extends PlainValueMember with AbstractValueMember
  case class PlainValueMemberDef(typeInst: TypeInst) extends PlainValueMember with ConcreteValueMember
  case class MethodMemberDecl(typeParams: TypeParams, typeInst: TypeInst) extends MethodMember with AbstractValueMember
  case class MethodMemberDef(typeParams: TypeParams, typeInst: TypeInst) extends MethodMember with ConcreteValueMember

  case class Members(types: Namespace[TypeCon], values: Namespace[ValueMember]) {
    def addValue(binding: (String, ValueMember)) = addOrShadowValue(binding, Members.empty)
    def addOrShadowValue(binding: (String, ValueMember), allowShadow: Members): An[Members] =
      values.addOrShadow(binding, allowShadow.values).toRight(Impl.error(s"Already defined: value member `${binding._1}`")).
      map(n => this.copy(values = n))
    def addType(binding: (String, TypeCon)) = addOrShadowType(binding, Members.empty)
    def addOrShadowType(binding: (String, TypeCon), allowShadow: Members): An[Members] =
      types.addOrShadow(binding, allowShadow.types).toRight(Impl.error(s"Already defined: type member `${binding._1}`")).
      map(t => this.copy(types = t))
  }

  object Members {
    def empty = Members(Namespace.empty[TypeCon], Namespace.empty[ValueMember])
  }

  def analyze(valueExpr: Ast.ValueExpr): An[TypeInst] = Impl.walkValueExpr(Env.empty)(valueExpr)

  object Impl {
    import Ast._

    //////
    // Helpers

    def error(e: String) = Seq(e)

    //////
    // General

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): An[TypeInst] = valueExpr match {
      case UnitLiteral() => instUnit(env)
      case IntegerLiteral(_) => instNullary(env)("Int")
      case StringLiteral(_) => instNullary(env)("String")
      case Tuple(es) => {
        An.seq(es.map(walkValueExpr(env))).flatMap(instTuple(env))
      }
      case block: Block => walkBlock(env)(block)
      case NamedValue(name) => env.useValue(name)
      case ValueAs(e, asType) =>
        An.join(walkValueExpr(env)(e), walkTypeInstExpr(env)(asType)).flatMap(expectSubTypeOf(env))
      case Lambda(parameter, body) =>
        walkPattern(env)(parameter, None).flatMap { case PatternResult(t, names) =>
          walkValueExpr(env/*.sub(names)*/)(body).flatMap { bodyType =>
            t match {
              case Some(paramType) => instFunction(env)((paramType, bodyType))
              case None => An.error(s"Must specify paramter type in lambda")
            }
          }
        }
      case MemberSelection(e, memberName, typeArgs) =>
        An.join(walkValueExpr(env)(e), walkTypeInstExprs(env)(typeArgs)).flatMap { case (inst, args) =>
          inst.valueMemberInst(env, memberName, args)
        }
      case Call(callee, Lambda(parameter, body)) => {
        // walkValueExpr(env)
        // callee must have apply member
        ???
      }
      case Call(originalCallee, argument) => {
        walkValueExpr(env)(originalCallee) flatMap { originalCalleeType =>
          walkValueExpr(env)(argument) flatMap { argType =>

            def recursiveDelegateCall(calleeType: TypeInst): An[TypeInst] = {
              calleeType match {
                case FuncTypeInst(con, paramType, resultType) =>
                  if (argType.isSubTypeOf(paramType)) {
                    An(resultType)
                  } else {
                    An.error(s"Type mismatch in call: argument was found to be ${argType.signature(env)}, expected parameter type is ${paramType.signature(env)}")
                  }
                case _ => calleeType.getValueMember("apply").map { applyMember =>
                  applyMember.inst(env, "apply", Nil).flatMap(recursiveDelegateCall(_))
                } getOrElse {
                  An.error(s"type `${calleeType.signature(env)}` cannot be called (does not have member `apply`)")
                }
              }
            }

            recursiveDelegateCall(originalCalleeType)
          }
        }
      }
      case ClassNew(namedType, astMembers) =>
        walkTypeInstExpr(env)(namedType) flatMap { ti =>
          if (astMembers.length == 0)
            An(ti)
          else
            walkMemberDecls(env)(Some(ti), astMembers) flatMap { members =>
              val name = "$AnonymousClass"
              val anonClass = ClassTypeCon(ClassHeaderTypeCon(name, TypeParams.empty, Some(ti)), members)

              anonClass.inst(env, Nil)
            }
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
        case Nil => currentNonUnitType.map(Right(_)).getOrElse(instUnit(currentEnv))
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
      case e: ValueExpr => An.join(instUnit(currentEnv), walkValueExpr(currentEnv)(e)).map { case (unit, t) => (currentEnv, Some(t).filter(_ !== unit))
      }
    }

    //////
    // ClassDef

    def walkClassDef(env: Env)(classDef: ClassDef): An[Env] = {
      val ClassDef(name, astTypeParams, parent, astMembers) = classDef

      walkTypeParams(env)(astTypeParams) flatMap { case (insideEnv, typeParams) =>
        walkClassParent(insideEnv)(parent) flatMap { parentInst =>
          val selfHead = ClassHeaderTypeCon(name, typeParams, parentInst)

          insideEnv.addType((name, selfHead)) flatMap { bodyEnv =>
            walkMemberDecls(bodyEnv)(parentInst, astMembers) flatMap { members =>
              env.addType((name, ClassTypeCon(selfHead, members)))
            }
          }
        }
      }
    }

    def walkTypeParams(env: Env)(typeParams: Seq[Ast.TypeParam]): An[(Env, TypeParams)] =
      typeParams.foldLeft(Nil: Errors, (env, TypeParams.empty)) { case ((currentErrors, (currentEnv, currentTypeParams)), typeParam) => {
        val Ast.TypeParam(variance, name, arity) = typeParam

        val varTypeCon = VarTypeCon(name, arity)
        currentEnv.addType((name, varTypeCon)) flatMap { updatedEnv =>
          currentTypeParams.add((name, varTypeCon)).
            toRight(error(s"Duplicate type parameter name $name")).
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

    def walkMemberDecls(env: Env)(parentInst: Option[TypeInst], members: Seq[MemberDecl]): An[Members] =
      members.foldLeft(WalkMemberDeclsState.begin(env, parentInst)) { case (state, memberDecl) =>
        state.step(memberDecl)
      }.end

    case class WalkMemberDeclsState(env: Env, parentInst: Option[TypeInst], currentErrors: Errors, currentDirectMembers: Members) {
      def noValueInParent(name: String): An[Unit] = parentInst.flatMap(_.getValueMember(name)) match {
          case Some(_) => An.error("sMember value `$name` already defined in parent")
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
        case ValueDecl(name, t) => noValueInParent(name).flatMap { _ =>
          walkTypeInstExpr(env)(t) flatMap { ti =>
            currentDirectMembers.addValue((name, PlainValueMemberDecl(ti)))
          }
        }
        case MethodDecl(name, typeParams, t) => noValueInParent(name).flatMap { _ =>
          walkTypeParams(env)(typeParams).flatMap { case (insideEnv, paramInsts) =>
            walkTypeInstExpr(insideEnv)(t) flatMap { ti =>
              currentDirectMembers.addValue((name, MethodMemberDecl(paramInsts, ti)))
            }
          }
        }
        case ValueDef(NamePattern(name), e) => parentInst.flatMap(_.getValueMember(name)) match {
            case Some(_: ConcreteValueMember) => An.error(s"Member already defined: `$name`")
            case Some(_: MethodMemberDecl) =>
              An.error(s"Member `$name` is declared to be a method.")
            case Some(PlainValueMemberDecl(expectedInst)) =>
              walkValueExpr(env)(e).flatMap { actualInst =>
                // TODO: maybe less generic error message
                expectSubTypeOf(env)((actualInst, expectedInst)).flatMap { _ =>
                  currentDirectMembers.addValue((name, PlainValueMemberDef(actualInst)))
                }
              }
            case None =>
              walkValueExpr(env)(e).flatMap { ti =>
                currentDirectMembers.addValue((name, PlainValueMemberDef(ti)))
              }
          }
        case complex: ValueDef => An.error("Error: Complex pattern not allowed in class member value declaration.")
        case MethodDef(name, typeParams, body) =>
          parentInst.flatMap(_.getValueMember(name)) match {
            case Some(_: ConcreteValueMember) => An.error(s"Member already defined: `$name`")
            case Some(_: PlainValueMemberDecl) =>
              An.error(s"Member `$name` is declared to be a plain value.")
            case Some(MethodMemberDecl(expectedTypeParams, expectedInst)) =>
              walkTypeParams(env)(typeParams).flatMap { case (insideEnv, actualTypeParams) =>
                // TODO def type params should be contravariant
                // TODO name difference should not matter
                if (actualTypeParams == expectedTypeParams) {
                  walkValueExpr(insideEnv)(body).flatMap { actualInst =>
                    expectSubTypeOf(env)((actualInst, expectedInst)).flatMap { _ =>
                      currentDirectMembers.addValue((name, MethodMemberDef(actualTypeParams, actualInst)))
                    }
                  }
                } else {
                  An.error(s"Type parameters do not match declaration.")
                }
              }
            case None =>
              walkTypeParams(env)(typeParams).flatMap { case (insideEnv, params) =>
                walkValueExpr(insideEnv)(body).flatMap { ti =>
                  currentDirectMembers.addValue((name, MethodMemberDef(params, ti)))
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

    //////
    // ---

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: TypeInst): An[Env] = target match {
      case NamePattern(name) => currentEnv.addOrShadowValue((name, t), parentEnv)
      case _ => ???
    }

    def walkTypeExpr(env: Env)(typeExpr: TypeExpr): An[TypeConOrTypeInst] = typeExpr match {
      case UnitType() => instUnit(env)
      case NamedType(name, typeArgs, pos) => walkNamedTypeExpr(env)(name, typeArgs)
      case FuncType(parameterExpr, resultExpr) =>
        An.tuple(mapPair(walkTypeInstExpr(env))((parameterExpr, resultExpr))).
          flatMap(instFunction(env))
      case TupleType(typeExprs) =>
        walkTypeInstExprs(env)(typeExprs).flatMap(instTuple(env))
    }

    def expectTypeInst(env: Env)(conOrInst: TypeConOrTypeInst): An[TypeInst] = conOrInst match {
      case con: TypeCon => An.error(s"Expected instantiated type but got type constructor ${con.signature(env)}")
      case inst: TypeInst => An(inst)
    }

    def walkTypeInstExpr(env: Env)(typeExpr: TypeExpr): An[TypeInst] =
      walkTypeExpr(env)(typeExpr).flatMap(expectTypeInst(env))

    def walkTypeInstExprs(env: Env)(typeExprs: Seq[TypeExpr]): An[Seq[TypeInst]] =
      An.seq(typeExprs.map(walkTypeInstExpr(env)))

    def walkNamedTypeExpr(env: Env)(name: String, typeArgs: Seq[TypeExpr]): An[TypeConOrTypeInst] =
      env.useType(name).flatMap { con =>
        if (con.arity > 0 && typeArgs.length == 0) {
          An(con)
        } else {
          An.seq(typeArgs.map(walkTypeExpr(env))) flatMap { args =>
            con.inst(env, args)
          }
        }
      }

    def instNullary(env: Env)(name: String): An[TypeInst] = env.useType(name).flatMap(_.inst(env, Nil))

    def instUnit(env: Env): An[TypeInst] = instNullary(env)("()")

    def instTuple(env: Env)(types: Seq[TypeInst]): An[TypeInst] =
      if (types.length == 2) {
        env.useType("Tuple").flatMap { con =>
          con.inst(env, types)
        }
      } else {
        An.error(s"Only 2-tuples are supported")
      }

    def instFunction(env: Env)(tt: (TypeInst, TypeInst)): An[TypeInst] =
      env.useType("Function").flatMap { con =>
        con.inst(env, Seq(tt._1, tt._2))
      }

    def expectSubTypeOf(env: Env)(tt: (TypeInst, TypeInst)): An[TypeInst] = {
      val (sub, sup) = tt

      if (sub.isSubTypeOf(sup))
        An(sub)
      else
        An.error(s"Type mismatch: found ${sub.signature(env)}, expected ${sup.signature(env)}")
    }

    case class PatternResult(t: Option[TypeInst], names: Namespace[TypeInst])

    def walkPattern(env: Env)(pattern: Pattern, actual: Option[TypeInst]): An[PatternResult] = pattern match {
      case UnitPattern() => instUnit(env).map(u => PatternResult(Some(u), Namespace.empty))
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
      //     case _ => An.error(s"Expected (?, ?) but got $actual")
      //   }
      // }
      // case NamePattern(name) => actual.map { at =>
      //   An(PatternResult(None, Namespace.empty.add((name, at))))
      // } getOrElse(An.error(s"Type must be specified: value `$name`"))
      // case PatternAs(p, asType) => walkTypeInstExpr(env)(asType) flatMap { asTypeInst =>
      //   walkPattern(env)(p, asTypeInst) flatMap { result =>
      //     (result.t.map { t =>
      //       expectSubTypeOf(t, asTypeInst)
      //     } getOrElse(An(asTypeInst))).map(PatternResult(_, result.names))
      //   }
      // }
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
