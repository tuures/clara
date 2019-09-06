package clara.jsemitter.impl

// JsAst => String

import ai.x.safe._

object JsPrinter {
  def emitString(node: JsAst.Node): String = JsPrinterImpl().walkNode(node)
}

case class JsPrinterImpl(indentChars: String = "  ", indentLevel: Int = 0) {
  import JsAst._

  def print(code: String): String = safe"${indentChars * indentLevel}${code}"

  def indented = this.copy(indentLevel = indentLevel + 1)

  def walkNode(node: Node) = node match {
    case e: Expr => walkExpr(e)
    case s: Stmt => walkStmt(s)
  }

  def walkExpr(expr: Expr) = expr match {
    case StringLiteral(value) => {
      val quoteChar = "'"

      print(safe"""${quoteChar}${value.replace(quoteChar, s"\\${quoteChar}")}${quoteChar}""")
    }
  }

  def walkStmt(expr: Stmt) = ???

}
