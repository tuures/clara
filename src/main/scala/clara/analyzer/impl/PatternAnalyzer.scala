package clara.analyzer.impl

import clara.asg.{TermLiteral, Terms, Types}
import clara.ast.{Ast, AstLiteral, Pos, SourceMessage}

case class PatternAnalyzer(env: Env, allowShadow: Env) {
  def walkAssignment(targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] = targetPattern match {
    case Ast.WildcardPattern(pos) =>
      val typ = fromType.getOrElse(Types.Top)
      An.result((env, Terms.WildcardPattern(typ)))
    case Ast.UnitPattern(pos) =>
      // FIXME remove repetition of this fromType check in all patterns
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
          (env, Terms.IntegerPattern(TermLiteral.integer(value), typ))
        }
      }
    case Ast.FloatPattern(AstLiteral.Float(whole, fraction), pos) =>
      TypeExprAnalyzer.namedNullaryType(env, "Float", pos).flatMap { typ =>
        fromType.fold(An.result(())){ fromType =>
          TypeInterpreter.expectAssignable(fromType, typ, pos)
        }.map { case () =>
          (env, Terms.FloatPattern(TermLiteral.Float(whole, fraction), typ))
        }
      }
    case Ast.StringPattern(parts, pos) => string(parts, fromType, pos)
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
    case Ast.NamedConstantPattern(name, pos) => namedConstant(name, fromType, pos)
    case Ast.CapturePattern(name, pos) => capture(name, fromType, pos)
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

  def string(
    parts: Seq[AstLiteral.StringPatternPart],
    fromType: Option[Types.Type],
    pos: Pos
  ): An[(Env, Terms.Pattern)] = TypeExprAnalyzer.namedNullaryType(env, "String", pos).flatMap { typ =>
    val initialState = (env, Vector.empty[TermLiteral.StringPatternPart])

    An.step(parts)(initialState) { case ((currentEnv, currentParts), part) =>
      part match {
        case AstLiteral.StringPlainPart(value) =>
          An.result((currentEnv, currentParts :+ TermLiteral.StringPlainPart(value)))
        case AstLiteral.StringEscapePart(escapes) =>
          An.result((currentEnv, currentParts :+ TermLiteral.StringEscapePart(escapes)))
        case AstLiteral.StringNamedConstantPart(Ast.NamedConstantPattern(name, pos)) =>
          PatternAnalyzer(currentEnv, allowShadow).namedConstant(name, Some(typ), pos).map { case (nextEnv, pattern) =>
            (nextEnv, currentParts :+ TermLiteral.StringNamedConstantPart(pattern))
          }
        case AstLiteral.StringCapturePart(Ast.CapturePattern(name, pos)) =>
          PatternAnalyzer(currentEnv, allowShadow).capture(name, Some(typ), pos).map { case (nextEnv, pattern) =>
            (nextEnv, currentParts :+ TermLiteral.StringCapturePart(pattern))
          }
      }
    }.flatMap { case (resultEnv, analyzedParts) =>
      fromType.fold(An.result(())){ fromType =>
        TypeInterpreter.expectAssignable(fromType, typ, pos)
      }.map { case () =>
        (resultEnv, Terms.StringPattern(analyzedParts, typ))
      }
    }
  }

  def namedConstant(name: String, fromType: Option[Types.Type], pos: Pos): An[(Env, Terms.NamedConstantPattern)] =
    ValueExprAnalyzer.namedValue(env, name, pos).flatMap { term =>
      fromType.fold(An.result(())){ fromType =>
        TypeInterpreter.expectAssignable(fromType, term.typ, pos)
      }.map { case () =>
        (env, Terms.NamedConstantPattern(term))
      }
    }

  def capture(name: String, fromType: Option[Types.Type], pos: Pos): An[(Env, Terms.CapturePattern)] =
    // FIXME default to Top type and just give warning?
    An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
      env.addOrShadowValue((name, fromType), allowShadow, pos).map { nextEnv =>
        (nextEnv, Terms.CapturePattern(name, fromType))
      }
    }

}
