package clara.analyzer

import clara.asg.Terms
import clara.ast.Ast
import clara.util.Message
import clara.util.Safe._

import impl._

// Ast => (Asg, Messages)
// Asg = Analyzed Semantic Graph

object Analyzer {
  def analyzeProgramBlock(programBlock: Ast.Block): (Either[Seq[Message], Terms.Block], Seq[Message]) = {
    val analysis: An[Terms.Block] = BlockAnalyzer.blockTerm(Env.empty, programBlock)

    (analysis.value, analysis.log)
  }
}
