package clara.jsemitter.impl

import scala.collection.immutable.ListMap

object JsAst {
  sealed trait Node
  sealed trait Expr extends Node
  sealed trait Stmt extends Node

  case object Undefined extends Expr
  case class NumberLiteral(value: String) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class ArrayLiteral(values: Seq[Expr]) extends Expr
  case class ObjectLiteral(props: ListMap[String, Expr]) extends Expr
  case class UnaryArrowFunc(param: String, body: Seq[Node]) extends Expr
  case class Named(name: String) extends Expr
  case class Member(e: Expr, name: String) extends Expr
  case class UnaryCall(target: Expr, argument: Expr) extends Expr

  case class Return(expr: Expr) extends Stmt
}
