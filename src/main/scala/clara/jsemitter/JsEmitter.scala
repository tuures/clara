package clara.jsemitter

// Asg => JsAst

import clara.asg.Asg

import impl._

object JsEmitter {
  def emitProgram(program: Asg.Block): JsAst.Node = {
    val body = program.bcs.flatMap(emitBlockContent)

    JsAst.UnaryCall(JsAst.UnaryArrowFunc("_global", body), JsAst.Named("global"))
  }

  def emitBlockContent(blockContent: Asg.BlockContent) = blockContent match {
    case e: Asg.ValueExpr => Some(emitValueExpr(e))
    case _: Asg.TypeDef => None
  }

  def emitValueExpr(valueExpr: Asg.ValueExpr) = valueExpr match {
    case _: Asg.UnitLiteral => JsAst.Undefined
  }

}
