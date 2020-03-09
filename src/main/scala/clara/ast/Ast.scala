package clara.ast

object Ast {

  sealed trait Node {
    val pos: Pos
  }

  sealed trait BlockContent extends Node
  sealed trait TypeExpr extends Node
  sealed trait Pattern extends Node
  sealed trait ValueExpr extends BlockContent
  sealed trait InBlockDef extends BlockContent
  sealed trait Method extends Node

  case class TopType(pos: Pos = NoPos) extends TypeExpr
  case class BottomType(pos: Pos = NoPos) extends TypeExpr

  case class UnitLiteral(pos: Pos = NoPos) extends ValueExpr
  case class UnitType(pos: Pos = NoPos) extends TypeExpr
  case class UnitPattern(pos: Pos = NoPos) extends Pattern

  case class IntegerLiteral(value: LiteralValue.Integer, pos: Pos = NoPos) extends ValueExpr

  case class FloatLiteral(value: LiteralValue.Float, pos: Pos = NoPos) extends ValueExpr

  case class StringLiteral(parts: Seq[LiteralValue.StringPart], pos: Pos = NoPos) extends ValueExpr

  case class Tuple(es: Seq[ValueExpr], pos: Pos = NoPos) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern], pos: Pos = NoPos) extends Pattern

  // case class Record(es: Seq[FieldDef], pos: Pos = NoPos) extends ValueExpr
  // case class RecordType(ts: Seq[FieldDecl], pos: Pos = NoPos) extends TypeExpr
  // case class RecordPattern(ps: Seq[FieldPattern], pos: Pos = NoPos) extends Pattern

  case class Block(bcs: Seq[BlockContent], pos: Pos = NoPos) extends ValueExpr

  case class NamedValue(name: String, pos: Pos = NoPos) extends ValueExpr
  case class NamedType(name: String, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class NamePattern(name: String, pos: Pos = NoPos) extends Pattern

  case class ValueAs(e: ValueExpr, t: TypeExpr, pos: Pos = NoPos) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr, pos: Pos = NoPos) extends Pattern

  case class Lambda(parameter: Pattern, body: ValueExpr, pos: Pos = NoPos) extends ValueExpr
  case class FuncType(parameter: TypeExpr, result: TypeExpr, pos: Pos = NoPos) extends TypeExpr

  case class NamedMember(name: String, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends Node
  case class MemberSelection(obj: ValueExpr, member: NamedMember, pos: Pos = NoPos) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, pos: Pos = NoPos) extends ValueExpr

  case class Attribute(key: String, value: Option[String], pos: Pos = NoPos) extends Node

  case class ValueNamesDef(target: Pattern, e: ValueExpr, pos: Pos = NoPos) extends InBlockDef

  case class TypeDef(name: String, t: TypeExpr, pos: Pos = NoPos) extends InBlockDef

  case class MethodDecl(attributes: Seq[Attribute], name: String, t: TypeExpr, pos: Pos = NoPos) extends Method
  case class MethodDef(attributes: Seq[Attribute], name: String, t: Option[TypeExpr], body: ValueExpr, pos: Pos = NoPos) extends Method

  // TODO: should we allow any type expr as target, not just named type?
  case class MethodSection(isDecl: Boolean, targetTypeName: String, methods: Seq[Method], pos: Pos = NoPos) extends InBlockDef


  // case class ValueDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends MemberDecl

  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
  case class TypeParam(variance: Variance, name: String, arity: Int, pos: Pos = NoPos) extends Node

  // case class MethodDecl(name: String, typeParams: Seq[TypeParam], t: TypeExpr, pos: Pos = NoPos) extends MemberDecl
  // case class MethodDef(name: String, typeParams: Seq[TypeParam], body: ValueExpr, pos: Pos = NoPos) extends MemberDef
  //
  // case class ClassDef(name: String, typeParams: Seq[TypeParam], parent: Option[NamedType], members: Seq[MemberDecl], pos: Pos = NoPos) extends InBlockDef
  //
  // // TODO members should probably be narrowed to Seq[ValueDef], requires dedicated parser rule, now Seq[MemberDecl] because classBody rule is reused
  // case class ClassNew(namedType: NamedType, members: Seq[MemberDecl], pos: Pos = NoPos) extends ValueExpr
}
