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
  case class WildcardPattern(pos: Pos = NoPos) extends Pattern

  case class UnitLiteral(pos: Pos = NoPos) extends ValueExpr
  case class UnitType(pos: Pos = NoPos) extends TypeExpr
  case class UnitPattern(pos: Pos = NoPos) extends Pattern

  case class IntegerLiteral(value: AstLiteral.Integer, pos: Pos = NoPos) extends ValueExpr
  case class IntegerPattern(value: AstLiteral.Integer, pos: Pos = NoPos) extends Pattern

  case class FloatLiteral(value: AstLiteral.Float, pos: Pos = NoPos) extends ValueExpr
  case class FloatPattern(value: AstLiteral.Float, pos: Pos = NoPos) extends Pattern

  case class StringLiteral(parts: Seq[AstLiteral.StringValuePart], pos: Pos = NoPos) extends ValueExpr
  case class StringPattern(parts: Seq[AstLiteral.StringPatternPart], pos: Pos = NoPos) extends Pattern

  case class Tuple(es: Seq[ValueExpr], pos: Pos = NoPos) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern], pos: Pos = NoPos) extends Pattern

  case class Block(bcs: Seq[BlockContent], pos: Pos = NoPos) extends ValueExpr

  case class NameWithPos(name: String, pos: Pos = NoPos) extends Node
  // TODO add typeArg for NamedValue or allow typeArg to be used on any expression with separate node?
  case class NamedValue(name: String, pos: Pos = NoPos) extends ValueExpr
  case class NamedType(name: NameWithPos, typeArgs: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  // TODO add escape syntax for lower case literal patterns
  case class NamedConstantPattern(name: String, pos: Pos = NoPos) extends Pattern
  case class CapturePattern(name: String, pos: Pos = NoPos) extends Pattern

  case class GuardPattern(p: Option[Pattern], guard: ValueExpr, pos: Pos = NoPos) extends Pattern
  case class DefaultValuePattern(p: Pattern, default: ValueExpr, pos: Pos = NoPos) extends Pattern

  // TODO rename? ValueExprTyped, PatternTyped
  case class ValueAs(e: ValueExpr, t: TypeExpr, pos: Pos = NoPos) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr, pos: Pos = NoPos) extends Pattern

  // FIXME NameWithPos
  case class FieldDef(name: String, t: Option[TypeExpr], body: ValueExpr, pos: Pos = NoPos) extends Node
  case class Record(fields: Seq[FieldDef], pos: Pos = NoPos) extends ValueExpr
  // FIXME NameWithPos
  case class FieldDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends Node
  case class RecordType(fields: Seq[FieldDecl], pos: Pos = NoPos) extends TypeExpr
  case class FieldPattern(name: String, t: Option[TypeExpr], subPattern: Option[Pattern], pos: Pos = NoPos) extends Node
  case class RecordPattern(fields: Seq[FieldPattern], pos: Pos = NoPos) extends Pattern

  case class UnionType(ts: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class IntersectionType(ts: Seq[TypeExpr], pos: Pos = NoPos) extends TypeExpr
  case class OrPattern(ps: Seq[Pattern], pos: Pos = NoPos) extends Pattern
  // TODO AndPattern? Is it useful enough to justify added complexity?

  // sealed trait Variance
  // case object Covariant extends Variance
  // case object Contravariant extends Variance
  // case object Invariant extends Variance
  // case class TypeParam(variance: Variance, name: String, arity: Int, pos: Pos = NoPos) extends Node
  case class TypeParam(name: String, pos: Pos = NoPos) extends Node
  case class Lambda(typeParams: Seq[TypeParam], parameter: Pattern, body: ValueExpr, pos: Pos = NoPos) extends ValueExpr
  case class Piecewise(pieces: Seq[(Pattern, ValueExpr)], pos: Pos = NoPos) extends ValueExpr
  case class FuncType(typeParams: Seq[TypeParam], parameter: TypeExpr, result: TypeExpr, pos: Pos = NoPos) extends TypeExpr

  case class MemberSelection(obj: ValueExpr, member: NamedValue, pos: Pos = NoPos) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, pos: Pos = NoPos) extends ValueExpr
  case class Pipe(argument: ValueExpr, callee: ValueExpr, pos: Pos = NoPos) extends ValueExpr

  case class ConstructPattern(name: NameWithPos, argumentPattern: Pattern, pos: Pos = NoPos) extends Pattern

  case class Attribute(key: String, value: Option[String], pos: Pos = NoPos) extends Node

  // TODO move?
  sealed trait TypeDefKind
  object TypeDefKind {
    sealed trait Wrapper extends TypeDefKind
    case object Alias extends Wrapper
    case object Tagged extends Wrapper
    case object Boxed extends Wrapper
    case object Opaque extends TypeDefKind
    case object Singleton extends TypeDefKind
  }

  case class DeclTargetType(name: NameWithPos, typeParams: Seq[TypeParam], pos: Pos = NoPos)

  // TODO: add attributes?
  case class TypeDef(typeDefKind: TypeDefKind, target: DeclTargetType, t: Option[TypeExpr], pos: Pos = NoPos) extends InBlockDecl

  // TODO: use ConstructPattern instead of targetType, selfPattern ?

  case class MethodDecl(attributes: Seq[Attribute], name: NameWithPos, t: TypeExpr, pos: Pos = NoPos) extends Method
  case class MethodDef(attributes: Seq[Attribute], name: NameWithPos, t: Option[TypeExpr], body: ValueExpr, pos: Pos = NoPos) extends Method
  case class MethodSection(isDecl: Boolean, target: DeclTargetType, selfPattern: Option[Pattern], methods: Seq[Method], pos: Pos = NoPos) extends InBlockDecl

  // TODO: add attributes?
  case class ValueDecl(name: String, t: TypeExpr, pos: Pos = NoPos) extends InBlockDecl
  case class ValueDef(target: Pattern, e: ValueExpr, pos: Pos = NoPos) extends InBlockDecl
}
