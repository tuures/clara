package clara

import collection.immutable.HashMap

case class Type()

object Analyzer {

  type Errors = Seq[String]

  import Parser._

  val types: HashMap[String, Type] = HashMap(
    "()" -> Type(),
    "Int" -> Type(),
    "String" -> Type(),
    "Tuple2" -> Type(),
    "Tuple3" -> Type(),
    "Function" -> Type()
  )

  def isSubtype(sub: TypeExpr, sup: TypeExpr) = sub == sup //FIXME

  def leastUpperBound(a: TypeExpr, b: TypeExpr) = ???

  def walkExpr(values: HashMap[String, TypeExpr])(ast: Node): Either[String, TypeExpr] = ast match {
    case ValueDef(target, e) => ???
    case UnitLiteral() => Right(UnitType())
    case IntegerLiteral(_) => Right(NamedType("Int"))
    case StringLiteral(_) => Right(NamedType("String"))
    case Tuple(es) => {
      val results = es.map(walkExpr(values))
      results collectFirst { case error @ Left(_) =>
        error
      } getOrElse {
        val types = results collect { case Right(typeExpr) =>
          typeExpr
        }
        Right(TupleType(types))
      }
    }
    case Block(es) => {
      val results = es.map(walkExpr(values))
      results collectFirst { case error @ Left(_) =>
        error
      } getOrElse results.last
    }
    case NamedValue(name) => values.get(name) match {
      case Some(typeExpr) => Right(typeExpr)
      case None => Left(s"Not found: value `$name`")
    }
    case ValueAs(e, asType) => walkExpr(values)(e) flatMap { actual =>
      if (isSubtype(actual, asType))
        Right(asType)
      else
        Left(s"Type mismatch: found $actual, expected $asType")
    }
    case Lambda(parameter, body) => {
      walkPattern(parameter, None) flatMap { paramType =>
        walkExpr(values)(body) map { resultType =>
          FuncType(paramType, resultType)
        }
      }
    }
    case _ => ???
  }

  def walkPattern(pattern: Pattern, fromAncestor: Option[TypeExpr]): Either[String, TypeExpr] = pattern match {
    case UnitPattern() => {
      val unit = UnitType()

      fromAncestor match {
        case Some(at) => {
          if (at == unit) // Unit cannot be subclassed
            Right(at)
          else
            Left(s"Pattern does not match expected type: found ${unit}, expected $at")
        }
        case None => Right(unit)
      }
    }
    case TuplePattern(ps) => {
      val results: Seq[Either[String, TypeExpr]] = fromAncestor map { at =>
        at match {
          case TupleType(ats) if ps.length == ats.length => { // tuples cannot be subclassed
            (ps zip ats) map { case (p, t) => walkPattern(p, Some(t)) }
          }
          case _ => Seq(Left(s"Pattern does not match expected type: found ${ps}, expected $at"))
        }
      } getOrElse ps.map(walkPattern(_, None))

      results collectFirst { case error @ Left(_) =>
        error
      } getOrElse {
        val types = results collect { case Right(typeExpr) =>
          typeExpr
        }
        Right(TupleType(types))
      }
    }
    case NamePattern(name) => fromAncestor match {
      case Some(t) => Right(t)
      case None => Left(s"Type must be specified: value `$name`")
    }
    case PatternAs(p, asType) => fromAncestor match {
      case Some(at) => {
        if (isSubtype(at, asType))
          walkPattern(p, Some(at))
        else
          Left(s"Type mismatch: found $asType, expected $fromAncestor")
      }
      case None => walkPattern(p, Some(asType))
    }
  }

}

object Clara {
  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import sext._

    val res = Parser.parse(input)

    import fastparse.core.Parsed
    res match {
      case Parsed.Success(v, index) =>
        println(v.treeString)
        println(Analyzer.walkExpr(HashMap.empty)(v))
      case Parsed.Failure(p, index, extra) =>
        println("Parse error: " + extra.traced.trace)
    }
  }
}
