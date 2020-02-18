package clara.jsemitter

// Asg => JsAst

import clara.asg.{Asg, Namespace}

import impl._

import ai.x.safe._
import scala.collection.immutable.ListMap

object JsEmitter {
  def emitProgram(program: Asg.Block): JsAst.Module = {
    val body = program.bcs.flatMap(emitBlockContent)

    // JsAst.UnaryCall(JsAst.UnaryArrowFunc("$", body), JsAst.Named("global"))
    val helpers = Seq(
      // JsAst.Const("$claraType", JsAst.UnaryCall(JsAst.Named("Symbol"), JsAst.StringLiteral("$claraType")))
    )

    JsAst.Module(helpers ++ body)
  }

  def emitBlockContent(blockContent: Asg.BlockContent) = blockContent match {
    case e: Asg.ValueExpr => Some(emitValueExpr(e))
    case _: Asg.ValueNamesDef => Some(emitValueNamesDef())
    case _: Asg.TypeDef => None
    case _: Asg.MethodDeclSection => None
    case Asg.MethodDefSection(typeName, targetType, methodDefs) => Some(emitMethodDefSection(typeName, targetType, methodDefs))
  }

  def emitValueExpr(valueExpr: Asg.ValueExpr) = valueExpr match {
    case _: Asg.UnitLiteral => JsAst.Undefined
  }

  def emitValueNamesDef() = ???

  def emitMethodDefSection(typeName: String, targetType: Asg.Typ, methodDefs: Namespace[Asg.MethodDef]) = {
    JsAst.Const(safe"${typeName}$$Methods", JsAst.ObjectLiteral(ListMap(???)))
  }

}
