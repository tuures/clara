package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, SourceMessage}

case class PatternAnalyzer(env: Env, allowShadow: Env) {
  def walkAssignment(targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] = targetPattern match {
    case Ast.UnitPattern(pos) =>
      // FIXME default to Bottom type to simplify
      (fromType match {
        case None => An.result(())
        case Some(fromType) =>
          TypeInterpreter.expectAssignable(fromType, Types.Uni, pos)
      }).map { case () =>
        (env, Terms.UnitPattern())
      }
    case _: Ast.TuplePattern => ???
    case Ast.NamePattern(name, pos) =>
      // FIXME default to Bottom type and just give warning?
      An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
        env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
          (nextEnv, Terms.NamePattern(name, fromType))
        }
      }
    case Ast.PatternAs(p, t, pos) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { targetType =>
        // FIXME default to Bottom type to simplify
        (fromType match {
          case None => An.result(())
          case Some(fromType) =>
            TypeInterpreter.expectAssignable(fromType, targetType, pos)
        }).flatMap { case () =>
          walkAssignment(p, Some(targetType))
        }
      }
  }
}
