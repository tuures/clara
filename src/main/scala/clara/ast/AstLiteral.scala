package clara.ast

object AstLiteral {
  sealed trait Integer
  // FIXME add sign
  case class IntegerBin(value: String) extends Integer
  case class IntegerDec(value: String) extends Integer
  case class IntegerHex(value: String) extends Integer

  // FIXME add sign
  case class Float(whole: String, fraction: String)

  sealed trait StringValuePart
  sealed trait StringPatternPart
  case class StringPlainPart(value: String) extends StringValuePart with StringPatternPart
  case class StringEscapePart(escapes: Seq[String]) extends StringValuePart with StringPatternPart {
    escapes.foreach { e =>
      assert(Seq("\\", "$", "\"", "n", "r", "t").contains(e) || e.startsWith("u"), s"invalid escape sequence: $e")
    }
  }
  case class StringValueExprPart(e: Ast.ValueExpr) extends StringValuePart
  case class StringNamedConstantPart(p: Ast.NamedConstantPattern) extends StringPatternPart
  case class StringCapturePart(p: Ast.CapturePattern) extends StringPatternPart
}
