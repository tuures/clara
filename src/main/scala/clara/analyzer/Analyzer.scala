package clara.analyzer

import clara.asg.Terms
import clara.ast.Ast
import clara.util.Message

import impl._

// Ast => (Asg, Messages)
// Asg = Analyzed Semantic Graph

case class AnalyzedProgram(analysis: An[Terms.Block]) {
  def program: Option[Terms.Block] = analysis.value.toOption
  def messages: Seq[Message] = analysis.log.warnings ++ analysis.value.left.getOrElse(Seq())
}

object Analyzer {
  def analyzeProgram(programBlock: Ast.Block): AnalyzedProgram =
    AnalyzedProgram(BlockAnalyzer.programBlockTerm(Env.empty, programBlock))
}
