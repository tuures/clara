package clara.analyzer

import clara.asg.Terms
import clara.ast.{Ast, SourceMessage}

import impl._

// Ast => (Asg, Messages)
// Asg = Analyzed Semantic Graph

case class AnalyzedProgram(result: AnResult[Terms.Block]) {
  def program: Option[Terms.Block] = result.value.toOption
  def messages: Seq[SourceMessage] = (result.log.warnings ++ result.value.left.getOrElse(Seq())).sortBy(_.pos)
}

object Analyzer {
  def analyzeProgram(programBlock: Ast.Block): AnalyzedProgram =
    AnalyzedProgram(BlockAnalyzer.programBlockTerm(Env.empty, programBlock).run)
}
