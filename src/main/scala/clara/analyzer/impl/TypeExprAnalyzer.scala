
package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.Ast

case class TypeExprAnalyzer(env: Env) {
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Types.Typ] = typeExpr match {
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.NamedType(name/*, typeArgs*/, pos) => env.useType(name, pos)
    case Ast.FuncType(parameter, result, pos) => {
      val tea = TypeExprAnalyzer(env)
      tea.walkTypeExpr(parameter).zip(tea.walkTypeExpr(result)).map { case (parameterTyp, resultTyp) =>
        Types.Func(parameterTyp, resultTyp)
      }
    }
  }
}
