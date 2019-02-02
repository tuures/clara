package clara

object Ast {

  sealed trait Node {
    val pos: Pos
  }

  sealed trait BlockContent extends Node
  sealed trait TypeExpr extends Node
  sealed trait Pattern extends Node
  sealed trait ValueExpr extends BlockContent
  sealed trait FreeDecl extends BlockContent
  sealed trait FreeDef extends FreeDecl
  sealed trait MemberDecl extends Node
  sealed trait MemberDef extends MemberDecl

  case class UnitLiteral(pos: Pos = NoPos) extends ValueExpr
  case class UnitType(pos: Pos = NoPos) extends TypeExpr
  case class UnitPattern(pos: Pos = NoPos) extends Pattern

  case class FloatLiteral(whole: String, fraction: String, pos: Pos = NoPos) extends ValueExpr

  sealed trait IntegerLiteralValue
  case class IntegerLiteralBinValue(value: String) extends IntegerLiteralValue
  case class IntegerLiteralDecValue(value: String) extends IntegerLiteralValue
  case class IntegerLiteralHexValue(value: String) extends IntegerLiteralValue
  case class IntegerLiteral(value: IntegerLiteralValue, pos: Pos = NoPos) extends ValueExpr

  sealed trait StringLiteralPart
  case class StringLiteralPlainPart(value: String) extends StringLiteralPart
  case class StringLiteralEscapePart(escapes: Seq[String]) extends StringLiteralPart
  case class StringLiteralExpressionPart(e: ValueExpr) extends StringLiteralPart
  case class StringLiteral(parts: Seq[StringLiteralPart], pos: Pos = NoPos) extends ValueExpr

  case class Tuple(es: Seq[ValueExpr], pos: Pos = NoPos) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern], pos: Pos = NoPos) extends Pattern

  case class Block(bcs: Seq[BlockContent], pos: Pos = NoPos) extends ValueExpr

  case class NamedValue(name: String, pos: Pos = NoPos) extends ValueExpr
  case class NamedType(name: String, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class NamePattern(name: String, pos: Pos = NoPos) extends Pattern

  case class ValueAs(e: ValueExpr, t: TypeExpr, pos: Pos = NoPos) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr, pos: Pos = NoPos) extends Pattern

  case class Lambda(parameter: Pattern, body: ValueExpr, pos: Pos = NoPos) extends ValueExpr
  case class FuncType(parameter: TypeExpr, result: TypeExpr, pos: Pos = NoPos) extends TypeExpr

  case class NamedMember(name: String, typeArgs: Seq[TypeExpr], pos: Pos = NoPos)
  case class MemberSelection(e: ValueExpr, member: NamedMember, pos: Pos = NoPos) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, pos: Pos = NoPos) extends ValueExpr

  case class ValueDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends MemberDecl
  case class ValueDef(target: Pattern, e: ValueExpr, pos: Pos = NoPos) extends FreeDef with MemberDef

  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
  case class TypeParam(variance: Variance, name: String, arity: Int, pos: Pos = NoPos) extends Node

  case class MethodDecl(name: String, typeParams: Seq[TypeParam], t: TypeExpr, pos: Pos = NoPos) extends MemberDecl
  case class MethodDef(name: String, typeParams: Seq[TypeParam], body: ValueExpr, pos: Pos = NoPos) extends MemberDef

  case class ClassDef(name: String, typeParams: Seq[TypeParam], parent: Option[NamedType], members: Seq[MemberDecl], pos: Pos = NoPos) extends FreeDef

  // TODO members should probably be narrowed to Seq[ValueDef], requires dedicated parser rule, now Seq[MemberDecl] because classBody rule is reused
  case class ClassNew(namedType: NamedType, members: Seq[MemberDecl], pos: Pos = NoPos) extends ValueExpr
}
