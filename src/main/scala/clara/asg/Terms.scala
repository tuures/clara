package clara.asg

import clara.asg.Attributes.MethodAttributes
import clara.asg.Types.Type
import clara.asg.TypeCons.TypeCon
import clara.ast.LiteralValue

object Terms {
  // program structure
  sealed trait BlockContent
  sealed trait Pattern {
    def typ: Type
  }
  sealed trait ValueExpr extends BlockContent {
    def typ: Type
  }
  sealed trait InBlockDecl extends BlockContent
  sealed trait Member {
    def attributes: MethodAttributes
  }

  case class UnitLiteral() extends ValueExpr {
    def typ = Types.Uni
  }
  case class UnitPattern() extends Pattern {
    def typ = Types.Uni
  }
  case class IntegerLiteral(value: LiteralValue.Integer, typ: Type) extends ValueExpr
  case class FloatLiteral(value: LiteralValue.Float, typ: Type) extends ValueExpr
  case class StringLiteral(parts: Seq[LiteralValue.StringPart], typ: Type) extends ValueExpr

  case class Tuple(es: Seq[ValueExpr], typ: Type) extends ValueExpr
  case class TuplePattern(ps: Seq[Pattern], typ: Type) extends Pattern

  case class Block(bcs: Seq[BlockContent], typ: Type) extends ValueExpr

  case class NamedValue(name: String, typ: Type) extends ValueExpr
  case class NamePattern(name: String, typ: Type) extends Pattern

  case class Field(body: ValueExpr)
  case class Record(fields: Namespace[Field], typ: Types.Record) extends ValueExpr

  case class Lambda(parameter: Pattern, body: ValueExpr, typ: Type) extends ValueExpr

  sealed trait SelectedMember
  case object SelectedField extends SelectedMember
  case class SelectedMethod(attributes: MethodAttributes) extends SelectedMember
  case class MemberSelection(obj: ValueExpr, memberName: String, selectedMember: SelectedMember, typ: Type) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, typ: Type) extends ValueExpr

  // FIMXE
  case class AliasTypeDef(name: String) extends InBlockDecl
  // FIXME
  case class TypeDef(con: TypeCon) extends InBlockDecl
  // FIXME
  case class NewExpr(typ: Type) extends ValueExpr

  sealed trait MethodSection extends InBlockDecl
  case class MethodDeclSection(targetType: Type, methodDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(attributes: MethodAttributes, typ: Type) extends Member
  case class MethodDefSection(targetType: Type, self: Pattern, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(attributes: MethodAttributes, body: ValueExpr) extends Member

  case class ValueDecl(name: String) extends InBlockDecl
  case class ValueDef(target: Pattern, e: ValueExpr) extends InBlockDecl
}
