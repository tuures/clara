package clara.analyzer

import clara.asg.Terms
import clara.ast.Ast
import clara.util.Message

import impl._

// Ast => (Asg, Messages)
// Asg = Analyzed Semantic Graph

case class AnalyzedProgram(result: AnResult[Terms.Block]) {
  def program: Option[Terms.Block] = result.value.toOption
  def messages: Seq[Message] = result.log.warnings ++ result.value.left.getOrElse(Seq())
}

object Analyzer {
  def analyzeProgram(programBlock: Ast.Block): AnalyzedProgram =
    AnalyzedProgram(BlockAnalyzer.programBlockTerm(Env.empty, programBlock).run)
}
