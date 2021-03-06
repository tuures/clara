package clara.asg

import clara.asg.Attributes.MethodAttributes
import clara.asg.Types.{MonoType, Typ}
import clara.ast.LiteralValue

object Terms {
  // program structure
  sealed trait BlockContent
  sealed trait Pattern
  sealed trait ValueExpr extends BlockContent {
    def typ: MonoType
  }
  sealed trait InBlockDef extends BlockContent
  sealed trait Member {
    def attributes: MethodAttributes
  }

  case class UnitLiteral() extends ValueExpr {
    def typ = Types.Uni
  }
  case class IntegerLiteral(value: LiteralValue.Integer, typ: MonoType) extends ValueExpr
  case class FloatLiteral(value: LiteralValue.Float, typ: MonoType) extends ValueExpr
  case class StringLiteral(parts: Seq[LiteralValue.StringPart], typ: MonoType) extends ValueExpr

  case class Block(bcs: Seq[BlockContent], typ: MonoType) extends ValueExpr

  case class NamedValue(name: String, typ: MonoType) extends ValueExpr
  case class NamePattern(name: String) extends Pattern

  case class Record(fields: Namespace[Field], typ: Types.Record) extends ValueExpr
  case class Field(body: ValueExpr)

  sealed trait SelectedMember
  case object SelectedField extends SelectedMember
  case class SelectedMethod(attributes: MethodAttributes) extends SelectedMember
  case class MemberSelection(obj: ValueExpr, memberName: String, selectedMember: SelectedMember, typ: MonoType) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, typ: MonoType) extends ValueExpr

  case class ValueDecl(name: String) extends InBlockDef
  case class ValueNamesDef(target: Pattern, e: ValueExpr) extends InBlockDef
  case class AliasTypeDef(name: String) extends InBlockDef
  case class TypeDef(name: String) extends InBlockDef
  case class NewExpr(typ: MonoType) extends ValueExpr

  sealed trait MethodSection extends InBlockDef
  case class MethodDeclSection(targetType: Typ, methodDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(attributes: MethodAttributes, typ: MonoType) extends Member
  case class MethodDefSection(targetType: Typ, self: Pattern, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(attributes: MethodAttributes, body: ValueExpr) extends Member
}
