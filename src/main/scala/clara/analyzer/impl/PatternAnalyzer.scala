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
    case Ast.TuplePattern(ps, pos) =>
      (fromType match {
        case None => An.result(ps.map(_ => None))
        case Some(Types.Tuple(fromTypes)) if fromTypes.length == ps.length => An.result(fromTypes.map(Some(_)))
        // FIXME instead of expectAssignable use something that returns the error directly
        case Some(typ) => TypeInterpreter.expectAssignable(typ, Types.Tuple(ps.map(_ => Types.Top)), pos).map(_ => ???)
      }).flatMap { fromTypes =>
          val initialState = (env, Vector.empty[Terms.Pattern])

          An.step(ps.zip(fromTypes))(initialState){ case ((currentEnv, currentPatternTerms), (pattern, fromType)) =>
            PatternAnalyzer(currentEnv, env).walkAssignment(pattern, fromType).map { case (nextEnv, patternTerm) =>
              (nextEnv, currentPatternTerms :+ patternTerm)
            }
          }.map { case(env, patternTerms) =>
            (env, Terms.TuplePattern(patternTerms, Types.Tuple(patternTerms.map(_.typ))))
          }
      }
    case Ast.NamePattern(name, pos) =>
      if (name.charAt(0).isUpper) {
        ValueExprAnalyzer.namedValue(env, name, pos).map { term =>
          (env, Terms.LiteralPattern(term))
        }
      } else {
        // FIXME default to Top type and just give warning?
        An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
          env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
            (nextEnv, Terms.CapturePattern(name, fromType))
          }
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
