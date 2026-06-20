package clara.asg

import clara.ast.AstLiteral

object TermLiteral {
  sealed trait Integer
  case class IntegerBin(value: String) extends Integer
  case class IntegerDec(value: String) extends Integer
  case class IntegerHex(value: String) extends Integer

  def integer(ast: AstLiteral.Integer): Integer = ast match {
    case AstLiteral.IntegerBin(v) => IntegerBin(v)
    case AstLiteral.IntegerDec(v) => IntegerDec(v)
    case AstLiteral.IntegerHex(v) => IntegerHex(v)
  }

  case class Float(whole: String, fraction: String)

  sealed trait StringValuePart
  sealed trait StringPatternPart

  case class StringPlainPart(value: String) extends StringValuePart with StringPatternPart
  case class StringEscapePart(escapes: Seq[String]) extends StringValuePart with StringPatternPart
  case class StringValueExprPart(expr: Terms.ValueExpr) extends StringValuePart
  case class StringNamedConstantPart(pattern: Terms.NamedConstantPattern) extends StringPatternPart
  case class StringCapturePart(pattern: Terms.CapturePattern) extends StringPatternPart
}
