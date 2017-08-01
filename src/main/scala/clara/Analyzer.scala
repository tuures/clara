package clara

import collection.immutable.HashMap

object Analyzer {

  type Errors = Seq[String]

  type EnvMap = HashMap[String, Type]
  case class Env(types: EnvMap, values: EnvMap) {
    def getValue(name: String) = values.get(name).toRight(Impl.error(s"Not found: value `$name`"))
    def getType(name: String) = types.get(name).toRight(Impl.error(s"Not found: type `$name`"))
    def getTypeName(t: Type) = types.map(_.swap).get(t).get // type instances should not exist outside envs
    def setValue(name: String, t: Type, allowShadow: Env = Env.empty) = values.get(name).flatMap { n =>
      if (allowShadow.values.get(name).isDefined) None else Some(n)
    }.toLeft(Env(types, values + (name -> t))).left.map(_ => Impl.error(s"Already defined: value `$name`"))
    def setType(name: String, t: Type) = types.get(name).toLeft(Env(types + (name -> t), values)).left.map(_ => Impl.error(s"Already defined: type `$name`"))
  }

  object Env {
    def empty = Env(HashMap.empty[String, Type], HashMap.empty[String, Type])
  }

  sealed trait Type {
    def toSource(env: Env): String
    def isSubTypeOf(other: Type): Boolean
  }
  class Type0(params: Seq[Type] = Nil) extends Type {
    def toSource(env: Env) = s"${env.getTypeName(this)}${(if (params.length > 0) params.mkString("[", ", ", "]") else "")}"
    def isSubTypeOf(other: Type) = this == other
  }
  // def leastUpperBound(a: TypeExpr, b: TypeExpr) = ???

  def analyze(env: Env)(valueExpr: Parser.ValueExpr): Either[Errors, Type] = Impl.walkValueExpr(env)(valueExpr)

  object Impl {
    import Parser._

    // def partitionEithers[Left : ClassTag, Right : ClassTag](in: Seq[Either[Left, Right]]): (Seq[Left], Seq[Right]) =
    //   in.partition(_.isLeft) match {
    //     case (leftList, rightList) =>
    //       (leftList.collect { case Left(l: Left) => l }, rightList.collect { case Right(r: Right) => r })
    // }

    def error(e: String) = Seq(e)

    def walkValueExpr(env: Env)(valueExpr: ValueExpr): Either[Errors, Type] = valueExpr match {
      case UnitLiteral() => env.getType("()")
      case IntegerLiteral(_) => env.getType("Int")
      case StringLiteral(_) => env.getType("String")
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
      case Member(e, member) => ???
      case Call(callee, argument) => ???
      case _ => ???
    }

    def walkBlockContents(env: Env)(bcs: Seq[BlockContent]): Either[Errors, Type] =
      bcs.foldLeft((env, Option.empty[Type], Nil: Errors)) { case ((currentEnv, currentNonUnitType, currentErrors), bc) =>
        val currentErrorsAndDiscard = currentErrors ++ (if (currentNonUnitType.isDefined) error(s"Warning: non unit value discarded in block") else Nil)

        bc match {
          case ValueDef(target, e) => {
            walkValueExpr(currentEnv)(e) match {
              case Right(t) => walkValueDef(env)(currentEnv, target, t) match {
                case Right(newEnv) => (newEnv, None, currentErrorsAndDiscard)
                case Left(errors) => (currentEnv, None, currentErrorsAndDiscard ++ errors)
              }
              case Left(errors) => (currentEnv, None, currentErrorsAndDiscard ++ errors)
            }
          }
          case e: ValueExpr => walkValueExpr(currentEnv)(e) match {
            case Right(t) if t != currentEnv.getType("()") => (currentEnv, Some(t), currentErrorsAndDiscard)
            case Right(t) => (currentEnv, None, currentErrorsAndDiscard)
            case Left(errors) => (currentEnv, None, currentErrorsAndDiscard ++ errors)
          }
        }
      } match {
        case (_, Some(t), Nil) => Right(t)
        case (_, None, Nil) => env.getType("()")
        case (_, _, errors) => Left(errors)
      }

    def walkValueDef(parentEnv: Env)(currentEnv: Env, target: Pattern, t: Type): Either[Errors, Env] = target match {
      case NamePattern(name) => currentEnv.setValue(name, t, allowShadow=parentEnv)
      case _ => ???
    }

    def walkTypeExpr(env: Env)(typeExpr: Node): Either[Errors, Type] = typeExpr match {
      case NamedType(name) => env.getType(name)
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
