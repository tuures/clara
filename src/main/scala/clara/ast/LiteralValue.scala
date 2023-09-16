package clara.ast

object LiteralValue {
  sealed trait Integer
  // FIXME add sign
  case class IntegerBin(value: String) extends Integer
  case class IntegerDec(value: String) extends Integer
  case class IntegerHex(value: String) extends Integer

  // FIXME add sign
  case class Float(whole: String, fraction: String)

  sealed trait StringPart
  case class StringPlainPart(value: String) extends StringPart
  case class StringEscapePart(escapes: Seq[String]) extends StringPart
  case class StringExpressionPart(e: Ast.ValueExpr) extends StringPart
}
