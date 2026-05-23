package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, SourceMessage}

case class PatternAnalyzer(env: Env, allowShadow: Env) {
  def walkAssignment(targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] = targetPattern match {
    case Ast.WildcardPattern(pos) =>
      val typ = fromType.getOrElse(Types.Top)
      An.result((env, Terms.WildcardPattern(typ)))
    case Ast.UnitPattern(pos) =>
      // FIXME default to Bottom type to simplify
      fromType.fold(An.result(())){ fromType =>
        TypeInterpreter.expectAssignable(fromType, Types.Uni, pos)
      }.map { case () =>
        (env, Terms.UnitPattern())
      }
    case Ast.IntegerPattern(value, pos) =>
      TypeExprAnalyzer.namedNullaryType(env, "Int", pos).flatMap { typ =>
        fromType.fold(An.result(())){ fromType =>
          TypeInterpreter.expectAssignable(fromType, typ, pos)
        }.map { case () =>
          (env, Terms.IntegerPattern(value, typ))
        }
      }
    case Ast.FloatPattern(value, pos) =>
      TypeExprAnalyzer.namedNullaryType(env, "Float", pos).flatMap { typ =>
        fromType.fold(An.result(())){ fromType =>
          TypeInterpreter.expectAssignable(fromType, typ, pos)
        }.map { case () =>
          (env, Terms.FloatPattern(value, typ))
        }
      }
    case Ast.StringPattern(parts, pos) =>
      val analyzedParts: An[Seq[Terms.StringPart]] = An.seq(parts.map {
        case clara.ast.LiteralValue.StringPlainPart(value) => An.result(Terms.StringPlainPart(value))
        case clara.ast.LiteralValue.StringEscapePart(escapes) => An.result(Terms.StringEscapePart(escapes))
        case clara.ast.LiteralValue.StringExpressionPart(e) => ??? // FIXME
      })
      analyzedParts.zip(TypeExprAnalyzer.namedNullaryType(env, "String", pos)).flatMap { case (analyzedParts, typ) =>
        fromType.fold(An.result(())){ fromType =>
          TypeInterpreter.expectAssignable(fromType, typ, pos)
        }.map { case () =>
          (env, Terms.StringPattern(analyzedParts, typ))
        }
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
    case Ast.NamedConstantPattern(name, pos) =>
      ValueExprAnalyzer.namedValue(env, name, pos).map { term =>
        (env, Terms.NamedConstantPattern(term))
      }
    case Ast.CapturePattern(name, pos) =>
      // FIXME default to Top type and just give warning?
      An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
        env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
          (nextEnv, Terms.CapturePattern(name, fromType))
        }
      }
    // TODO: this actually changes the type of the nested pattern to targetType even if it's wider than fromType
    // in ValueExpr the ValueAs does not change the type. This should be made consistent either way.
    case Ast.PatternAs(p, t, _) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { targetType =>
        // TODO default fromType to Bottom type to simplify?
        fromType.fold(An.result(())){ fromType =>
          TypeInterpreter.expectAssignable(fromType, targetType, t.pos)
        }.flatMap { case () =>
          walkAssignment(p, Some(targetType))
        }
      }
    //TODO OrPattern |, AndPattern &
    case Ast.ConstructPattern(name, selfPattern, pos) => ???
  }
}
