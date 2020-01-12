
package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.Ast

case class TypeExprAnalyzer(env: Env) {
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Asg.Typ] = typeExpr match {
    case Ast.UnitType(_) => An.result(Asg.Uni)
    case Ast.NamedType(name, typeArgs, pos) => env.useType(name, pos)
    case Ast.FuncType(parameter, result, pos) => {
      val tea = TypeExprAnalyzer(env)
      tea.walkTypeExpr(parameter).zip(tea.walkTypeExpr(result)).map { case (parameterTyp, resultTyp) =>
        Asg.Func(parameterTyp, resultTyp)
      }
    }
  }
}
