package clara.ast

object Ast {

  sealed trait Node {
    val pos: Pos
  }

  sealed trait BlockContent extends Node
  sealed trait TypeExpr extends Node
  sealed trait Pattern extends Node
  sealed trait Method extends Node
  sealed trait ValueExpr extends BlockContent
  sealed trait InBlockDecl extends BlockContent

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

  case class Block(bcs: Seq[BlockContent], pos: Pos = NoPos) extends ValueExpr

  case class NameWithPos(name: String, pos: Pos = NoPos) extends Node
  case class NamedValue(name: String, pos: Pos = NoPos) extends ValueExpr
  case class NamedType(name: NameWithPos, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class NamePattern(name: String, pos: Pos = NoPos) extends Pattern // rename to CapturePattern?

  // TODO rename? ValueExprTyped, PatternTyped
  case class ValueAs(e: ValueExpr, t: TypeExpr, pos: Pos = NoPos) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr, pos: Pos = NoPos) extends Pattern

  case class Record(fields: Seq[FieldDef], pos: Pos = NoPos) extends ValueExpr
  // FIXME NameWithPos
  case class FieldDef(name: String, t: Option[TypeExpr], body: ValueExpr, pos: Pos = NoPos) extends Node
  case class RecordType(fields: Seq[FieldDecl], pos: Pos = NoPos) extends TypeExpr
  // FIXME add sign
  case class FieldDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends Node
  // TODO case class RecordPattern(fields: Seq[FieldPattern], pos: Pos = NoPos) extends Pattern
  // TODO FieldPattern = NamePattern | PatternTyped(NamePattern, t)
  // TODO support nested patterns? like: {point: Point {x, y}, size: Int} => ...
  // TODO support renaming? like: {point @ p, size: Int} => ...
  // TODO how to support default values in patterns? {size: Int = 1}

  case class Lambda(/*typeParams: Seq[TypeParam], */parameter: Pattern, body: ValueExpr, pos: Pos = NoPos) extends ValueExpr
  case class FuncType(/*typeParams: Seq[TypeParam], */parameter: TypeExpr, result: TypeExpr, pos: Pos = NoPos) extends TypeExpr

  case class NamedMember(name: String, pos: Pos = NoPos) extends Node
  case class MemberSelection(obj: ValueExpr, member: NamedMember, pos: Pos = NoPos) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr/*, typeArgs: Seq[TypeExpr]*/, pos: Pos = NoPos) extends ValueExpr

  //case class ConstructPattern(targetType: TypeName, selfPattern: Pattern, pos: Pos = NoPos) extends Pattern

  case class Attribute(key: String, value: Option[String], pos: Pos = NoPos) extends Node

  sealed trait TypeDefKind
  object TypeDefKind {
    case object Alias extends TypeDefKind
    case object Tagged extends TypeDefKind
    case object Boxed extends TypeDefKind
    case object Opaque extends TypeDefKind
    case object Singleton extends TypeDefKind
  }

  // sealed trait Variance
  // case object Covariant extends Variance
  // case object Contravariant extends Variance
  // case object Invariant extends Variance
  // case class TypeParam(variance: Variance, name: String, arity: Int, pos: Pos = NoPos) extends Node
  case class TypeParam(name: String, pos: Pos = NoPos) extends Node

  // TODO: add attributes?
  case class TypeDef(typeDefKind: TypeDefKind, name: NameWithPos, typeParams: Seq[TypeParam], t: TypeExpr, pos: Pos = NoPos) extends InBlockDecl

  // TODO: remove duplication MethodDeclSection vs MethodDefSection
  // TODO: use ConstructPattern instead of targetType, selfPattern

  case class MethodDeclSection(targetType: NameWithPos, methods: Seq[Method], pos: Pos = NoPos) extends InBlockDecl
  case class MethodDecl(attributes: Seq[Attribute], name: String, t: TypeExpr, pos: Pos = NoPos) extends Method
  case class MethodDefSection(targetType: NameWithPos, selfPattern: Pattern, methods: Seq[Method], pos: Pos = NoPos) extends InBlockDecl
  case class MethodDef(attributes: Seq[Attribute], name: String, t: Option[TypeExpr], body: ValueExpr, pos: Pos = NoPos) extends Method

  // TODO: add attributes?
  case class ValueDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends InBlockDecl
  case class ValueDef(target: Pattern, e: ValueExpr, pos: Pos = NoPos) extends InBlockDecl

  case class InvalidBlockContent(invalidSyntax: String, pos: Pos = NoPos) extends BlockContent
}
