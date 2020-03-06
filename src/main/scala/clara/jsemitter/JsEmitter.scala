package clara.jsemitter

// Asg => JsAst

import clara.asg.{Terms, Types, Namespace}
import clara.ast.LiteralValue

import impl._

import ai.x.safe._
import scala.collection.immutable.ListMap

object JsEmitter {
  def emitProgram(program: Terms.Block): JsAst.Module = {
    val body = program.bcs.flatMap(emitBlockContent)

    // JsAst.UnaryCall(JsAst.UnaryArrowFunc("$", body), JsAst.Named("global"))
    val helpers = Seq(
      // JsAst.Const("$claraType", JsAst.UnaryCall(JsAst.Named("Symbol"), JsAst.StringLiteral("$claraType")))
    )

    JsAst.Module(helpers ++ body)
  }

  def emitValueExpr(valueExpr: Terms.ValueExpr): JsAst.Expr = valueExpr match {
    case _: Terms.UnitLiteral => JsAst.Undefined
    case Terms.IntegerLiteral(value, _) => emitIntegerLiteral(value)
    case Terms.FloatLiteral(value, _) => emitFloatLiteral(value)
    case Terms.StringLiteral(parts, _) => emitStringLiteral(parts)
    case Terms.Block(bcs, _) => emitBlock(bcs)
    case Terms.NamedValue(name, _) => JsAst.Named(name)
    case Terms.MemberSelection(obj, memberName, member, _) => emitMemberSelection(obj, memberName, member)
    case Terms.Call(Terms.MemberSelection(obj, memberName, member, _), argument, _) if member.attributes.emitBinaryOperator =>
      emitBinaryOperation(obj, memberName, member, argument)
    case Terms.Call(callee, argument, _) => emitCall(callee, argument)
  }

  def emitIntegerLiteral(value: LiteralValue.Integer) = value match {
    case LiteralValue.IntegerBin(value) => ???
    case LiteralValue.IntegerDec(value) => JsAst.NumberLiteral(value)
    case LiteralValue.IntegerHex(value) => ???
  }

  def emitFloatLiteral(value: LiteralValue.Float) = ???

  def emitStringLiteral(parts: Seq[LiteralValue.StringPart]) = ???

  def emitBlock(bcs: Seq[Terms.BlockContent]) = {
    JsAst.NullaryCall(JsAst.NullaryArrowFunc(bcs.flatMap(emitBlockContent)))
  }

  def emitBlockContent(blockContent: Terms.BlockContent): Option[JsAst.Node] = blockContent match {
    case e: Terms.ValueExpr => Some(emitValueExpr(e))
    case Terms.ValueNamesDef(target, e) => Some(emitValueNamesDef(target, e))
    case _: Terms.TypeDef => None
    case _: Terms.MethodDeclSection => None
    case Terms.MethodDefSection(typeName, targetType, methodDefs) => Some(emitMethodDefSection(typeName, targetType, methodDefs))
  }

  def emitValueNamesDef(target: Terms.Pattern, e: Terms.ValueExpr) = target match {
    case Terms.NamePattern(name) => JsAst.Const(name, emitValueExpr(e))
  }

  def emitMethodDefSection(typeName: String, targetType: Types.Typ, methodDefs: Namespace[Terms.MethodDef]) = {
    JsAst.Const(safe"${typeName}$$Methods", JsAst.ObjectLiteral(ListMap(???)))
  }

  def emitMemberName(memberName: String, member: Terms.Member): String = member.attributes.emitName.getOrElse(memberName)

  def emitMemberSelection(obj: Terms.ValueExpr, memberName: String, member: Terms.Member) = {
    val name = emitMemberName(memberName, member)
    member.attributes.emitBinaryOperator match {
      case false => JsAst.Member(emitValueExpr(obj), name)
      case true => JsAst.UnaryArrowFunc("_", Seq(JsAst.BinaryOperation(name, emitValueExpr(obj), JsAst.Named("_"))))
    }
  }

  def emitBinaryOperation(obj: Terms.ValueExpr, memberName: String, member: Terms.Member, argument: Terms.ValueExpr) =
    JsAst.BinaryOperation(emitMemberName(memberName, member), emitValueExpr(obj), emitValueExpr(argument))

  def emitCall(callee: Terms.ValueExpr, argument: Terms.ValueExpr) =
    JsAst.UnaryCall(emitValueExpr(callee), emitValueExpr(argument))
}
