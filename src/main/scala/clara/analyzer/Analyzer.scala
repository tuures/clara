package clara.analyzer

import clara.asg.Asg
import clara.ast.Ast
import clara.util.Message

import impl._

// Ast => (Asg, Messages)

object Analyzer {
  def analyzeProgramBlock(programBlock: Ast.Block): Either[Seq[Message], Asg.Block] = {
    val Ast.Block(bcs, pos) = programBlock
    val result: An[Asg.Block] = BlockAnalyzer(Env.empty).walkBlock(bcs, pos)
    require(result.w.log.length == 0, "warnings not implemented")
    result.w.value
  }
}
