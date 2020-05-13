package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.Ast

case class PatternAnalyzer(env: Env, allowShadow: Env) {
  def walkAssignment(targetPattern: Ast.Pattern, fromType: Types.Typ): An[(Terms.Pattern, Env)] = targetPattern match {
    // case Ast.UnitPattern(pos) => An.result((Terms.UnitPattern, env))
    // case Ast.TuplePattern
    case Ast.NamePattern(name, pos) => env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
      (Terms.NamePattern(name), nextEnv)
    }
    case Ast.PatternAs(p, t, pos) =>
      TypeExprAnalyzer(env).walkTypeExpr(t).flatMap { typ =>
        TypeAnalyzer.expectAssignable(fromType, typ, pos).flatMap { _: Unit =>
          walkAssignment(p, typ)
        }
      }
  }
}
