package clara.analyzer.impl

import clara.asg.Types
import clara.ast.Ast

case class TypeExprAnalyzer(env: Env) {
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Types.Typ] = typeExpr match {
    case Ast.TopType(_) => An.result(Types.Top)
    case Ast.BottomType(_) => An.result(Types.Bottom)
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.TupleType(ts, pos) => ???
    case Ast.NamedType(name/*, typeArgs*/, pos) => env.useType(name, pos)
    case Ast.FuncType(parameter, result, pos) => {
      val tea = TypeExprAnalyzer(env)
      tea.walkTypeExpr(parameter).zip(tea.walkTypeExpr(result)).map { case (parameterTyp, resultTyp) =>
        Types.Func(parameterTyp, resultTyp)
      }
    }
  }
}
