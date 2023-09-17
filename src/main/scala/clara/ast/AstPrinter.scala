package clara.ast

import clara.util.Safe._

case class AstPrinterImpl(indentLevel: Int = 0) {
  def indented = this.copy(indentLevel = indentLevel + 1)

  def print(x: Any): Option[String] = x match {
    case s: Seq[_] => Some(s.flatMap(v => print(v)).mkString("\n")).filter(_.nonEmpty)
    case p: Product if p.isInstanceOf[Ast.Node] => {
      val name = p.productPrefix
      val children = Some(p.productIterator.toSeq.flatMap(v => indented.print(v)).mkString("\n")).filter(_.nonEmpty)

      Some(s"${" " * indentLevel}+$name${children.map(c => safe"\n$c").getOrElse("")}")
    }
    case _: Pos => None
    case _ => Some(s"${" " * indentLevel} ${x.toString}")
  }
}

object AstPrinter {
  def print(astNode: Ast.Node): String = AstPrinterImpl().print(astNode).getOrElse("")
}
