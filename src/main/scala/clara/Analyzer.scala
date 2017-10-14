package clara

import collection.immutable.HashMap

import ai.x.safe._

object Analyzer {

  type Errors = Seq[String]

  case class Env(types: HashMap[String, TypeCon], values: HashMap[String, TypeInst]) {
    def getValue(name: String) = values.get(name).toRight(Impl.error(s"Not found: value `$name`"))
    def getTypeCon(name: String) = types.get(name).toRight(Impl.error(s"Not found: type `$name`"))
    def getNullaryTypeInst(name: String) = getTypeCon(name).flatMap(c => c.inst())
    def getTypeName(t: TypeCon): String = types.map(_.swap).get(t).get // TypeCon should not exist outside envs
    def setValue(name: String, t: TypeInst, allowShadow: Env = Env.empty) = values.get(name).flatMap { n =>
      if (allowShadow.values.get(name).isDefined) None else Some(n)
    }.toLeft(Env(types, values + (name -> t))).left.map(_ => Impl.error(s"Already defined: value `$name`"))
    def setType(name: String, t: TypeCon) = types.get(name).toLeft(Env(types + (name -> t), values)).left.map(_ => Impl.error(s"Already defined: type `$name`"))
  }

  object Env {
    def empty = Env(HashMap.empty[String, TypeCon], HashMap.empty[String, TypeInst])
  }

  class TypeCon(val members: Map[String, TypeInst], params: Seq[String] = Nil) {
    def toSource(env: Env) = s"${env.getTypeName(this)}${(if (params.length > 0) params.mkString("[", ", ", "]") else "")}"
    def inst(args: Seq[TypeInst] = Nil): Either[Errors, TypeInst] = {
      if (args.length == params.length)
        Right(TypeInst(this, args))
      else
        Left(Impl.error("invalid number of type arguments"))
    }
  }

  case class TypeInst(con: TypeCon, args: Seq[TypeInst]) {
    def members = con.members // FIXME
    def toSource(env: Env) = s"${env.getTypeName(con)}${(if (args.length > 0) args.mkString("[", ", ", "]") else "")}"
    def isSubTypeOf(other: TypeInst) = this == other
  }

  // def leastUpperBound(a: TypeExpr, b: TypeExpr) = ???

  def analyze(env: Env)(valueExpr: Parser.ValueExpr): Either[Errors, TypeInst] = Impl.walkValueExpr(env)(valueExpr)

  object Impl {
    import Parser._

    // def partitionEithers[Left : ClassTag, Right : ClassTag](in: Seq[Either[Left, Right]]): (Seq[Left], Seq[Right]) =
    //   in.partition(_.isLeft) match {
    //     case (leftList, rightList) =>
    //       (leftList.collect { case Left(l: Left) => l }, rightList.collect { case Right(r: Right) => r })
    // }

    def error(e: String) = Seq(e)

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): Either[Errors, TypeInst] = valueExpr match {
      case UnitLiteral() => env.getNullaryTypeInst("()")
      case IntegerLiteral(_) => env.getNullaryTypeInst("Int")
      case StringLiteral(_) => env.getNullaryTypeInst("String")
      // case Tuple(es) => {
      //   val results = es.map(walkValueExpr(env))
      //
      //   partitionEithers(results).map(types => )
      //
      // }
      case Block(bcs) => walkBlockContents(env)(bcs)
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
      case Member(e, member) => walkValueExpr(env)(e) flatMap { target =>
        target.members.get(member).toRight(error(s"type ${target.toSource(env)} does not have member $member"))
      }
      case Call(callee, argument) => ???
      case _ => ???
    }

    def walkBlockContents(env: Env)(bcs: Seq[BlockContent]): Either[Errors, TypeInst] =
      bcs.foldLeft((Nil: Errors, Option.empty[TypeInst], env)) { case ((currentErrors, currentNonUnitType, currentEnv), bc) =>
        val currentErrorsAndDiscard = currentErrors ++ (if (currentNonUnitType.isDefined) error(s"Warning: non unit value discarded in block") else Nil)

        walkBlockContent(env, currentEnv, bc) match {
          case Right((nextEnv, nonUnitType)) => (currentErrorsAndDiscard, nonUnitType, nextEnv)
          case Left(errors) => (currentErrorsAndDiscard ++ errors, None, currentEnv)
        }
      } match {
        case (Nil, nonUnitType, _) => nonUnitType.map(Right(_)).getOrElse(env.getNullaryTypeInst("()"))
        case (errors, _, _) => Left(errors)
      }

    def walkBlockContent(parentEnv: Env, currentEnv: Env, bc: BlockContent): Either[Errors, (Env, Option[TypeInst])] = bc match {
      case ValueDef(target, e) =>
        walkValueExpr(currentEnv)(e) flatMap { t =>
          walkValueDef(parentEnv)(currentEnv, target, t)
        } map (newEnv => (newEnv, None))
      case ClassDef(name, contents) =>
        (contents.foldLeft((Nil: Errors, Map.empty[String, TypeInst])) { case ((currentErrors, members), cc) =>
          cc match {
            case ClassMember(name, t) =>
              walkTypeExpr(currentEnv)(t) match {
                case Right(ti) => (currentErrors, members + (name -> ti))
                case Left(errors) => (currentErrors ++ errors, members)
              }
          }
        }) match {
          case (Nil, members) => {
            currentEnv.setType(name, new TypeCon(members)) map (newEnv => (newEnv, None))
          }
          case (errors, _) => {
            Left(errors)
          }
        }
      case e: ValueExpr =>
        currentEnv.getNullaryTypeInst("()") flatMap { unit =>
          walkValueExpr(currentEnv)(e) map { t =>
            (currentEnv, Some(t).filter(_ !== unit))
          }
        }
    }

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: TypeInst): Either[Errors, Env] = target match {
      case NamePattern(name) => currentEnv.setValue(name, t, allowShadow=parentEnv)
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
