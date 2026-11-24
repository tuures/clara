package clara.analyzer

import clara.asg.Terms
import clara.ast.{Ast, SourceMessage}

import impl._

// Ast => (Asg, Messages)
// Asg = Analyzed Semantic Graph

case class AnalyzedModule(result: AnResult[Terms.Block]) {
  def module: Option[Terms.Block] = result.value.toOption
  def messages: Seq[SourceMessage] = (result.log.warnings ++ result.value.left.getOrElse(Seq())).sortBy(_.pos)
}

object Analyzer {
  def analyzeModule(moduleBlock: Ast.Block): AnalyzedModule =
    AnalyzedModule(BlockAnalyzer.moduleBlockTerm(Env.empty, moduleBlock).run)
}
