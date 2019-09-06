package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.Ast

case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Asg.ValueExpr] = ???
}
