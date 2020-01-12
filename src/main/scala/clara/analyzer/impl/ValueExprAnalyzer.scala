package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.Ast

case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Asg.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Asg.UnitLiteral())
    case Ast.Block(bcs, pos) => BlockAnalyzer(env).walkBlock(bcs, pos)
    case Ast.NamedValue(name, pos) => env.useValue(name, pos).map(typ => Asg.NamedValue(name, typ))
  }
}
