package clara.jsemitter.impl

// JsAst => String

import ai.x.safe._

object JsPrinter {
  def emitString(module: JsAst.Module): String = JsPrinterImpl().walkModule(module)
}

case class JsPrinterImpl(i: String = "  ") {
  import JsAst._

  val nli = safe"\n$i"

  def walkModule(module: Module) = module.nodes.map(walkNode).safeMkString("\n\n")

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
    case NullaryArrowFunc(body) => walkArrowFunc("()", body)
    case UnaryArrowFunc(param, body) => walkArrowFunc(param, body)
    case Member(obj, memberName) => safe"${walkExpr(obj)}.$memberName"
    case NullaryCall(target) => walkCall(target, "")
    case UnaryCall(target, argument) => walkCall(target, walkExpr(argument))
    case BinaryOperation(operator, a, b) => walkBinaryOperation(operator, a, b)
  }

  def walkObjectLiteral(entries: Seq[(String, Expr)]): String =
    entries.map { case (name, expr) => safe"$name: ${walkExpr(expr)}" }.safeMkString(safe"{\n$i", safe",\n$i", "\n}")

  def walkArrowFunc(param: String, body: Seq[Node]): String = body match {
    case Seq((e: JsAst.Expr)) => safe"$param =>$nli" + walkExpr(e) + "\n"
    case _ => {
      safe"$param => {" + body.map(walkNode).safeMkString(nli, nli, "") + "\n}"
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
    case Const(name, expr) => safe"const $name = ${walkExpr(expr)}"
  }

}
