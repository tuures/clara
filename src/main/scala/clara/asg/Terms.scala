package clara.asg

import clara.asg.Attributes.MethodAttributes
import clara.asg.Types.Type
import clara.asg.TypeCons.TypeCon
import clara.asg.TermLiteral

object Terms {
  sealed trait Node extends Product

  // program structure
  sealed trait BlockContent extends Node
  sealed trait Pattern extends Node {
    def typ: Type
  }
  sealed trait ValueExpr extends BlockContent {
    def typ: Type
  }
  sealed trait InBlockDecl extends BlockContent
  sealed trait Member extends Node {
    def attributes: MethodAttributes
  }

  case class WildcardPattern(typ: Type) extends Pattern

  case class UnitLiteral() extends ValueExpr {
    def typ = Types.Uni
  }
  case class UnitPattern() extends Pattern {
    def typ = Types.Uni
  }

  case class IntegerLiteral(value: TermLiteral.Integer, typ: Type) extends ValueExpr
  case class IntegerPattern(value: TermLiteral.Integer, typ: Type) extends Pattern

  case class FloatLiteral(value: TermLiteral.Float, typ: Type) extends ValueExpr
  case class FloatPattern(value: TermLiteral.Float, typ: Type) extends Pattern

  case class StringLiteral(parts: Seq[TermLiteral.StringValuePart], typ: Type) extends ValueExpr
  case class StringPattern(parts: Seq[TermLiteral.StringPatternPart], typ: Type) extends Pattern

  case class Tuple(es: Seq[ValueExpr], typ: Type) extends ValueExpr
  case class TuplePattern(ps: Seq[Pattern], typ: Type) extends Pattern

  case class Block(bcs: Seq[BlockContent], typ: Type) extends ValueExpr

  case class NamedValue(name: String, typ: Type) extends ValueExpr

  case class NamedConstantPattern(term: NamedValue) extends Pattern {
    def typ: Type = term.typ
  }
  case class CapturePattern(name: String, typ: Type) extends Pattern

  case class Field(body: ValueExpr)
  case class Record(fields: Namespace[Field], typ: Types.Record) extends ValueExpr
  // TODO case class RecordPattern(fields: Namespace[Pattern], typ: Types.Record) extends Pattern

  // TODO OrPattern |, AndPattern &
  case class Lambda(parameter: Pattern, body: ValueExpr, typ: Type) extends ValueExpr

  // FIXME
  case class Piecewise(pieces: Seq[(Pattern, ValueExpr)], typ: Type) extends ValueExpr

  sealed trait SelectedMember extends Node
  case object SelectedField extends SelectedMember
  case class SelectedMethod(targetCon: TypeCon, attributes: MethodAttributes) extends SelectedMember
  case class MemberSelection(obj: ValueExpr, memberName: String, selectedMember: SelectedMember, typ: Type) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, typ: Type) extends ValueExpr

  // TODO case class ConstructPattern(con: TypeCon, args: Seq[Pattern], typ: Type) extends Pattern

  // TODO con type could be narrowed to rule out ParamCon
  case class TypeDef(con: TypeCon) extends InBlockDecl

  sealed trait MethodSection extends InBlockDecl
  case class MethodDeclSection(targetCon: TypeCon, methodDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(attributes: MethodAttributes, typ: Type) extends Member
  case class MethodDefSection(targetCon: TypeCon, self: Pattern, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(attributes: MethodAttributes, body: ValueExpr) extends Member

  case class ValueDecl(name: String) extends InBlockDecl
  case class ValueDef(target: Pattern, e: ValueExpr) extends InBlockDecl
}
