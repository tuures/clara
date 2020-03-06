package clara.jsemitter.impl

import scala.collection.immutable.ListMap

object JsAst {
  sealed trait Node
  sealed trait Expr extends Node
  sealed trait Stmt extends Node
  sealed trait Defi extends Node

  case object Undefined extends Expr
  case class NumberLiteral(value: String) extends Expr
  case class StringLiteral(value: String) extends Expr
  case class ArrayLiteral(values: Seq[Expr]) extends Expr
  case class ObjectLiteral(props: ListMap[String, Expr]) extends Expr
  case class Named(name: String) extends Expr
  sealed trait ArrowFunc extends Expr
  case class NullaryArrowFunc(body: Seq[Node]) extends ArrowFunc
  case class UnaryArrowFunc(param: String, body: Seq[Node]) extends ArrowFunc
  case class Member(obj: Expr, memberName: String) extends Expr
  case class NullaryCall(target: Expr) extends Expr
  case class UnaryCall(target: Expr, argument: Expr) extends Expr
  case class BinaryOperation(operator: String, a: Expr, b: Expr) extends Expr

  case class Return(expr: Expr) extends Stmt

  case class Const(name: String, e: Expr) extends Defi

  case class Module(nodes: Seq[Node])
}
