package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.Ast

case class PatternAnalyzer(env: Env, parentEnv: Env) {
  def walkPattern(pattern: Ast.Pattern, againstType: Types.Typ): An[(Terms.Pattern, Env)] = pattern match {
    case Ast.NamePattern(name, pos) => env.addOrShadowValue((name, againstType), parentEnv, pos).map { nextEnv =>
      (Terms.NamePattern(name), nextEnv)
    }
    case _ => ???
  }
}
