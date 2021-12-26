package clara.jsemitter.impl

// JsAst => String

import clara.util.Safe._

object JsPrinter {
  def emitString(module: JsAst.Module): String = JsPrinterImpl().walkModule(module)
}

case class JsPrinterImpl() {

  def indented(s: String) = s.replaceAll("(^|\n)", "$1  ")

  import JsAst._

  def walkModule(module: Module) = module.nodes.map(walkNode).safeString("\n\n")

  def walkNode(node: Node): String = node match {
    case e: Expr => walkExpr(e)
    case s: Stmt => walkStmt(s)
    case d: Defi => walkDefi(d)
  }

  def walkExpr(expr: Expr): String = expr match {
    case Undefined => "undefined"
    case NumberLiteral(value) => value
    case StringLiteral(value) => {
      val quoteChar = "'"

      safe"""${quoteChar}${value.replace(quoteChar, s"\\${quoteChar}")}${quoteChar}"""
    }
    case ArrayLiteral(values) => ???
    case ObjectLiteral(entries) => walkObjectLiteral(entries)
    case Named(name) => name
    case NullaryArrowFunc(body) => walkArrowFunc(None, body)
    case UnaryArrowFunc(param, body) => walkArrowFunc(Some(param), body)
    case Member(obj, memberName) => safe"${walkExpr(obj)}.$memberName"
    case NullaryCall(target) => walkCall(target, "")
    case UnaryCall(target, argument) => walkCall(target, walkExpr(argument))
    case BinaryOperation(operator, a, b) => walkBinaryOperation(operator, a, b)
  }

  def walkObjectLiteral(entries: Seq[(String, Expr)]): String =
    entries.map { case (name, expr) => indented(safe"$name: ${walkExpr(expr)}") }.safeString("{\n", ",\n", "\n}")

  def walkArrowFunc(param: Option[Pattern], body: Seq[Node]): String = {
    val paramPrinted = param.map(walkPattern).getOrElse("()")

    body match {
      case Seq((e: JsAst.Expr)) => safe"$paramPrinted =>\n" + indented(walkExpr(e))
      case _ => {
        safe"$paramPrinted => {\n" + indented(body.map(walkNode).safeString("\n")) + "\n}"
      }
    }
  }

  def walkCall(target: Expr, argumentPrinted: String) = {
    val targetPrinted = walkExpr(target)
    val wrappedTarget = target match {
      case _: ArrowFunc => safe"($targetPrinted)"
      case _ => targetPrinted
    }

    wrappedTarget + safe"(${argumentPrinted})"
  }

  def walkBinaryOperand(e: Expr): String = e match {
    // wrap inner binary operations in parens to avoid problems with precendence
    case BinaryOperation(operator, a, b) => safe"(${walkBinaryOperation(operator, a, b)})"
    case _ => walkExpr(e)
  }

  def walkBinaryOperation(operator: String, a: Expr, b: Expr): String = {
    safe"${walkBinaryOperand(a)} $operator ${walkBinaryOperand(b)}"
  }

  def walkStmt(stmt: Stmt): String = stmt match {
    case Return(expr) => safe"return ${walkExpr(expr)}"
  }

  def walkDefi(defi: Defi): String = defi match {
    case Const(target, expr) => safe"const ${walkPattern(target)} = ${walkExpr(expr)}"
  }

  def walkPattern(pattern: Pattern): String = pattern match {
    case NamePattern(name) => name
  }

}
