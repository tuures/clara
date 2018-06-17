package clara

object Ast {
  case class SourceInfo(name: String, lineIndices: Seq[Int])
  sealed trait Pos
  case class RangePos(source: SourceInfo, from: Int, until: Int) extends Pos
  case object NoPos extends Pos

  sealed trait Node {
    // val pos: Pos
  }
  sealed trait BlockContent extends Node
  sealed trait TypeExpr extends Node
  sealed trait Pattern extends Node
  sealed trait ValueExpr extends BlockContent
  sealed trait FreeDecl extends BlockContent
  sealed trait FreeDef extends FreeDecl
  sealed trait MemberDecl extends Node
  sealed trait MemberDef extends MemberDecl
  case class UnitLiteral() extends ValueExpr
  case class UnitType() extends TypeExpr
  case class UnitPattern() extends Pattern
  case class IntegerLiteral(value: String) extends ValueExpr
  case class StringLiteral(value: String) extends ValueExpr
  case class Tuple(es: Seq[ValueExpr]) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr]) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern]) extends Pattern
  case class Block(bcs: Seq[BlockContent]) extends ValueExpr
  case class NamedValue(name: String) extends ValueExpr
  case class NamedType(name: String, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class NamePattern(name: String) extends Pattern
  case class ValueAs(e: ValueExpr, t: TypeExpr) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr) extends Pattern
  case class Lambda(parameter: Pattern, body: ValueExpr) extends ValueExpr
  case class FuncType(parameter: TypeExpr, result: TypeExpr) extends TypeExpr
  case class MemberSelection(e: ValueExpr, memberName: String, typeArgs: Seq[TypeExpr]) extends ValueExpr
  case class Call(callee: ValueExpr, argument: ValueExpr) extends ValueExpr
  case class ValueDecl(name: String, t: TypeExpr) extends MemberDecl
  case class ValueDef(target: Pattern, e: ValueExpr) extends FreeDef with MemberDef
  case class TypeParam(variance: Variance, name: String, arity: Int) extends Node
  case class MethodDecl(name: String, typeParams: Seq[TypeParam], t: TypeExpr) extends MemberDecl
  case class MethodDef(name: String, typeParams: Seq[TypeParam], body: ValueExpr) extends MemberDef
  case class ClassDef(name: String, typeParams: Seq[TypeParam], parent: Option[NamedType], members: Seq[MemberDecl]) extends FreeDef
  case class ClassNew(namedType: NamedType, members: Seq[MemberDecl]) extends ValueExpr
  case class Comment(text: String) extends BlockContent

  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
}
