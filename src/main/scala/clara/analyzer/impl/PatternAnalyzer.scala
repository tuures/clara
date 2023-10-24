package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.Ast

case class PatternAnalyzer(env: Env, allowShadow: Env) {
  def walkAssignment(targetPattern: Ast.Pattern, fromType: Types.Type): An[(Terms.Pattern, Env)] = targetPattern match {
    // case Ast.UnitPattern(pos) => An.result((Terms.UnitPattern, env))
    // case Ast.TuplePattern
    case Ast.NamePattern(name, pos) => env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
      (Terms.NamePattern(name), nextEnv)
    }
    case Ast.PatternAs(p, t, pos) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { typ =>
        TypeInterpreter.expectAssignable(fromType, typ, pos).flatMap { case () =>
          walkAssignment(p, typ)
        }
      }
  }
}
