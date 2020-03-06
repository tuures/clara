package clara.asg

import clara.ast.LiteralValue

object Terms {
  import Types.Typ

  // program structure
  sealed trait BlockContent
  sealed trait Pattern
  sealed trait ValueExpr extends BlockContent {
    def typ: Typ
  }
  sealed trait InBlockDef extends BlockContent
  sealed trait Member {
    def attributes: MemberAttributes
  }

  case class UnitLiteral() extends ValueExpr {
    def typ = Types.Uni
  }
  case class IntegerLiteral(value: LiteralValue.Integer, typ: Typ) extends ValueExpr
  case class FloatLiteral(value: LiteralValue.Float, typ: Typ) extends ValueExpr
  case class StringLiteral(parts: Seq[LiteralValue.StringPart], typ: Typ) extends ValueExpr

  case class Block(bcs: Seq[BlockContent], typ: Typ) extends ValueExpr

  case class NamedValue(name: String, typ: Typ) extends ValueExpr
  case class NamePattern(name: String) extends Pattern

  case class MemberAttributes(emitBinaryOperator: Boolean = false, emitName: Option[String] = None)
  case class MemberSelection(obj: ValueExpr, memberName: String, member: Member, typ: Typ) extends ValueExpr

  case class Call(callee: ValueExpr, argument: ValueExpr, typ: Typ) extends ValueExpr

  case class ValueNamesDef(target: Pattern, e: ValueExpr) extends InBlockDef
  case class TypeDef(name: String) extends InBlockDef

  sealed trait MethodSection extends InBlockDef
  case class MethodDeclSection(targetType: Typ, methodDecls: Namespace[MethodDecl]) extends MethodSection
  case class MethodDecl(attributes: MemberAttributes, typ: Typ) extends Member
  case class MethodDefSection(typeName: String, targetType: Typ, methodDefs: Namespace[MethodDef]) extends MethodSection
  case class MethodDef(attributes: MemberAttributes, body: ValueExpr) extends Member

}
