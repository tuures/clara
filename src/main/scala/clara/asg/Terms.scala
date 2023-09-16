package clara.asg

import clara.asg.Attributes.MethodAttributes
import clara.asg.Types.Type
import clara.ast.LiteralValue

object Terms {
  // program structure
  sealed trait BlockContent
  sealed trait Pattern
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
  case class IntegerLiteral(value: LiteralValue.Integer, typ: Type) extends ValueExpr
  case class FloatLiteral(value: LiteralValue.Float, typ: Type) extends ValueExpr
  case class StringLiteral(parts: Seq[LiteralValue.StringPart], typ: Type) extends ValueExpr

  case class Block(bcs: Seq[BlockContent], typ: Type) extends ValueExpr

  case class NamedValue(name: String, typ: Type) extends ValueExpr
  case class NamePattern(name: String) extends Pattern

  case class Record(fields: Namespace[Field], typ: Types.Record) extends ValueExpr
  case class Field(body: ValueExpr)

  sealed trait SelectedMember
  case object SelectedField extends SelectedMember
  case class SelectedMethod(attributes: MethodAttributes) extends SelectedMember
  case class MemberSelection(obj: ValueExpr, memberName: String, selectedMember: SelectedMember, typ: Type) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, typ: Type) extends ValueExpr

  case class AliasTypeDef(name: String) extends InBlockDecl
  case class TypeDef(name: String) extends InBlockDecl
  case class NewExpr(typ: Type) extends ValueExpr

  sealed trait MethodSection extends InBlockDecl
  case class MethodDeclSection(targetType: Type, methodDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(attributes: MethodAttributes, typ: Type) extends Member
  case class MethodDefSection(targetType: Type, self: Pattern, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(attributes: MethodAttributes, body: ValueExpr) extends Member

  case class ValueDecl(name: String) extends InBlockDecl
  case class ValueDef(target: Pattern, e: ValueExpr) extends InBlockDecl
}
