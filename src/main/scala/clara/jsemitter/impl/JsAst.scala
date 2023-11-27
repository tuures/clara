package clara.jsemitter.impl

object JsAst {
  sealed trait Node
  sealed trait Content extends Node
  sealed trait Expr extends Content
  sealed trait Stmt extends Content
  sealed trait Defi extends Content
  sealed trait Pattern extends Node

  case object Undefined extends Expr
  case class NumberLiteral(value: String) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class ArrayLiteral(values: Seq[Expr]) extends Expr
  case class ArrayPattern(ps: Seq[Pattern]) extends Pattern
  case class ObjectLiteral(entries: Seq[(String, Expr)]) extends Expr
  case class Named(name: String) extends Expr
  case object UnitPattern extends Pattern
  case class NamePattern(name: String) extends Pattern
  sealed trait ArrowFunc extends Expr
  case class NullaryArrowFunc(body: Seq[Content]) extends ArrowFunc
  case class UnaryArrowFunc(param: Pattern, body: Seq[Content]) extends ArrowFunc
  case class Member(obj: Expr, memberName: String) extends Expr
  case class NullaryCall(target: Expr) extends Expr
  case class UnaryCall(target: Expr, argument: Expr) extends Expr
  case class BinaryOperation(operator: String, a: Expr, b: Expr) extends Expr

  case class Return(expr: Expr) extends Stmt

  case class Const(target: Pattern, e: Expr) extends Defi

  case class Module(nodes: Seq[Content]) extends Node
}
