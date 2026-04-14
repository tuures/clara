package clara.jsemitter.impl

// JsAst => String

import clara.util.Safe._

object JsPrinter {
  def printModule(module: JsAst.Module): String = JsPrinterImpl.printModule(module)
}

object JsPrinterImpl {

  def indented(s: String) = s.replaceAll("(^|\n)", "$1  ")

  import JsAst._

  def printModule(module: Module) = module.nodes.map(printContent).safeString("\n\n")

  def printContent(c: Content): String = c match {
    case e: Expr => printExpr(e)
    case s: Stmt => printStmt(s)
    case d: Defi => printDefi(d)
  }

  def arraySyntax(elements: Seq[String]) = elements.safeString("[", ", ", "]")

  def printExpr(expr: Expr): String = expr match {
    case Undefined => "undefined"
    case NumberLiteral(value) => value
    case StringLiteral(value) => {
      val quoteChar = "'"

      safe"""${quoteChar}${value.replace(quoteChar, safe"\\${quoteChar}")}${quoteChar}"""
    }
    case ArrayLiteral(values) => arraySyntax(values.map(printExpr))
    case ObjectLiteral(entries) => printObjectLiteral(entries)
    case Named(name) => name
    case UnaryArrowFunc(param, body) => printArrowFunc(param, body)
    case Iife(body) => safe"(${printArrowFunc(UnitPattern, body)})()"
    case Member(obj, memberName) => safe"${printExpr(obj)}.$memberName"
    case UnaryCall(target, argument) => printCall(target, printExpr(argument))
    case BinaryOperation(operator, a, b) => printBinaryOperation(operator, a, b)
  }

  def printObjectLiteral(entries: Seq[(String, Expr)]): String = entries match {
    case Nil => "{}"
    case _ => entries.map { case (name, expr) => indented(safe"$name: ${printExpr(expr)}") }.safeString("{\n", ",\n", "\n}")
  }

  def printBlock(body: Seq[Content]) = "{\n" + indented(body.map(printContent).safeString("\n"))+ "\n}"

  def printArrowFunc(param: Pattern, body: Seq[Content]): String = {
    val paramPrinted = param match {
      case UnitPattern => "()"
      case _: ArrayPattern => safe"(${printPattern(param)})"
      case _ => printPattern(param)
    }

    def printArrayExprBody(e: Expr): String = safe"$paramPrinted =>\n" + indented(printExpr(e))
    def printArrayBlockBody(contents: Seq[Content]): String = safe"$paramPrinted => ${printBlock(contents)}"

    body match {
      case Seq(single) => single match {
        case Return(e) => e match {
          case Iife(innerBody) => printArrayBlockBody(innerBody)
          case _ => printArrayExprBody(e)
        }
        case e: JsAst.Expr => e match {
          case Iife(innerBody) => printArrayBlockBody(innerBody)
          case _ => printArrayExprBody(e)
        }
        case _ => printArrayBlockBody(body)
      }
      case _ => {
        printArrayBlockBody(body)
      }
    }
  }

  def printCall(target: Expr, argumentPrinted: String) = {
    val targetPrinted = printExpr(target)
    val wrappedTarget = target match {
      case _: ArrowFunc | _: Iife => safe"($targetPrinted)"
      case _ => targetPrinted
    }

    wrappedTarget + safe"(${argumentPrinted})"
  }

  def printBinaryOperand(e: Expr): String = e match {
    // wrap inner binary operations in parens to avoid problems with precendence
    case BinaryOperation(operator, a, b) => safe"(${printBinaryOperation(operator, a, b)})"
    case _ => printExpr(e)
  }

  def printBinaryOperation(operator: String, a: Expr, b: Expr): String = {
    safe"${printBinaryOperand(a)} $operator ${printBinaryOperand(b)}"
  }

  def printStmt(stmt: Stmt): String = stmt match {
    case Return(expr) => safe"return ${printExpr(expr)}"
    case If(ifBranches, elseBranch) => {
      val ifs = ifBranches.zipWithIndex.map { case (IfBranch(predicate, body), index) =>
        val keyword = if (index > 0) "else if" else "if"
        safe"$keyword (${printExpr(predicate)}) ${printBlock(body)}"
      }
      val maybeElse = if (elseBranch.nonEmpty) {
        Seq(safe"else ${printBlock(elseBranch)}")
      } else Nil

      (ifs ++ maybeElse).safeString(" ")
    }
  }

  def printDefi(defi: Defi): String = defi match {
    case Const(UnitPattern, expr) => printExpr(expr)
    case Const(target, expr) => safe"const ${printPattern(target)} = ${printExpr(expr)}"
  }

  def printPattern(pattern: Pattern): String = pattern match {
    case UnitPattern => ""
    case ArrayPattern(ps) => arraySyntax(ps.map(printPattern))
    case NamePattern(name) => name
  }

}
