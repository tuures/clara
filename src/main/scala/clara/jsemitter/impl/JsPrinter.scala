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
    case StringLiteral(value) => {
      val quoteChar = "'"

      safe"""${quoteChar}${value.replace(quoteChar, s"\\${quoteChar}")}${quoteChar}"""
    }
    case Named(name) => name
    case UnaryArrowFunc(param, body) => {
      body match {
        case Seq((e: JsAst.Expr)) => safe"$param =>$nli" + walkExpr(e) + "\n"
        case _ => {
          safe"$param => {" + body.map(walkNode).safeMkString(nli, nli, "") + "\n}"
        }
      }
    }
    case UnaryCall(target, argument) => {
      val targetPrinted = walkExpr(target)
      val wrappedTarget = target match {
        case _: UnaryArrowFunc => safe"($targetPrinted)"
        case _ => targetPrinted
      }

      wrappedTarget + safe"(${walkExpr(argument)})"
    }
  }

  def walkStmt(stmt: Stmt): String = stmt match {
    case Return(expr) => safe"return ${walkExpr(expr)}"
  }

  def walkDefi(defi: Defi): String = defi match {
    case Const(name, expr) => safe"const $name = ${walkExpr(expr)}"
  }

}
