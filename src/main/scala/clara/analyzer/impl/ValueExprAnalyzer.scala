package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.Ast

case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Asg.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(pos) => env.useType("Unit", pos).map { typeCon =>
      Asg.UnitLiteral(typeCon.inst(), pos)
    }
  }
}
