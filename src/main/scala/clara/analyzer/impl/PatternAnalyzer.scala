package clara.analyzer.impl

import clara.asg.{TermLiteral, Terms, Types}
import clara.ast.{Ast, AstLiteral, Pos, SourceMessage}

import clara.util.Safe._

// refutable: whether the pattern only has to be able to match some values (a case of #()) instead of every value
// (an assignment or a function parameter), see matchType. Applies to the whole pattern including nested patterns.
case class PatternAnalyzerImpl(env: Env, refutable: Boolean) {
  def walk(targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] = targetPattern match {
    case Ast.WildcardPattern(pos) =>
      val typ = fromType.getOrElse(Types.Top)
      An.result((env, Terms.WildcardPattern(typ)))
    case Ast.UnitPattern(pos) =>
      matchType(fromType, Types.Uni, pos).map { _ =>
        (env, Terms.UnitPattern())
      }
    case Ast.IntegerPattern(value, pos) =>
      expectRefutable(pos).flatMap { case () =>
        TypeExprAnalyzer.namedNullaryType(env, "Int", pos).flatMap(matchType(fromType, _, pos))
      }.map { typ =>
        (env, Terms.IntegerPattern(TermLiteral.integer(value), typ))
      }
    case Ast.FloatPattern(AstLiteral.Float(whole, fraction), pos) =>
      expectRefutable(pos).flatMap { case () =>
        TypeExprAnalyzer.namedNullaryType(env, "Float", pos).flatMap(matchType(fromType, _, pos))
      }.map { typ =>
        (env, Terms.FloatPattern(TermLiteral.Float(whole, fraction), typ))
      }
    case Ast.StringPattern(parts, pos) => parts match {
      // a single capture matches every string
      case Seq(_: AstLiteral.StringCapturePart) => string(parts, fromType, pos)
      case _ => expectRefutable(pos).flatMap { case () => string(parts, fromType, pos) }
    }
    case Ast.TuplePattern(ps, pos) =>
      (fromType match {
        case None => An.result(ps.map(_ => None))
        case Some(fromType) => matchType(Some(fromType), Types.Tuple(ps.map(_ => Types.Top)), pos).map { _ =>
          Types.tupleElementTypes(fromType, ps.length).map(Some(_))
        }
      }).flatMap { fromTypes =>
        val initialState = (env, Vector.empty[Terms.Pattern])

        An.step(ps.zip(fromTypes))(initialState){ case ((currentEnv, currentPatternTerms), (pattern, fromType)) =>
          copy(env = currentEnv).walk(pattern, fromType).map { case (nextEnv, patternTerm) =>
            (nextEnv, currentPatternTerms :+ patternTerm)
          }
        }.map { case(env, patternTerms) =>
          (env, Terms.TuplePattern(patternTerms, Types.Tuple(patternTerms.map(_.typ))))
        }
      }
    case Ast.NamedConstantPattern(name, pos) => namedConstant(name, fromType, pos)
    case Ast.CapturePattern(name, pos) => capture(name, fromType, pos)
    case Ast.GuardPattern(p, guard, pos) => ???
    case Ast.DefaultValuePattern(p, default, pos) => ???
    // TODO: this actually changes the type of the nested pattern to targetType even if it's wider than fromType
    // in ValueExpr the ValueAs does not change the type. This should be made consistent either way.
    case Ast.PatternAs(p, t, pos) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { targetType =>
        // a case quotes the whole pattern, an assignment points at the type annotation the value doesn't fit
        matchType(fromType, targetType, if (refutable) pos else t.pos).flatMap { typ =>
          walk(p, Some(typ))
        }
      }
    case Ast.RecordPattern(fields, pos) => ???
    case Ast.OrPattern(ps, pos) => ???
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
          copy(env = currentEnv).namedConstant(name, Some(typ), pos).map { case (nextEnv, pattern) =>
            (nextEnv, currentParts :+ TermLiteral.StringNamedConstantPart(pattern))
          }
        case AstLiteral.StringCapturePart(Ast.CapturePattern(name, pos)) =>
          copy(env = currentEnv).capture(name, Some(typ), pos).map { case (nextEnv, pattern) =>
            (nextEnv, currentParts :+ TermLiteral.StringCapturePart(pattern))
          }
      }
    }.flatMap { case (resultEnv, analyzedParts) =>
      matchType(fromType, typ, pos).map { matchedType =>
        (resultEnv, Terms.StringPattern(analyzedParts, matchedType))
      }
    }
  }

  def namedConstant(name: String, fromType: Option[Types.Type], pos: Pos): An[(Env, Terms.NamedConstantPattern)] =
    ValueExprAnalyzer.namedValue(env, name, pos).flatMap { term =>
      matchType(fromType, term.typ, pos).map { _ =>
        (env, Terms.NamedConstantPattern(term))
      }
    }

  def capture(name: String, fromType: Option[Types.Type], pos: Pos): An[(Env, Terms.CapturePattern)] =
    // FIXME default to Top type and just give warning?
    An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
      env.addOrShadowValue(name, fromType, pos).map { nextEnv =>
        (nextEnv, Terms.CapturePattern(name, fromType))
      }
    }

  // Checks that a pattern matching values of patternType can match a value of fromType and returns the type of the
  // matched value. An irrefutable pattern has to match every value of fromType, its type stays patternType.
  // A refutable pattern only has to be able to match some value of fromType and the type narrows to the values it
  // matches, e.g. 1 on Int | String narrows to Int.
  def matchType(fromType: Option[Types.Type], patternType: Types.Type, pos: Pos): An[Types.Type] = fromType match {
    case None => An.result(patternType)
    case Some(fromType) if refutable =>
      if (Types.disjoint(fromType, patternType)) {
        val pattern = quotedPattern(pos)
        An.error(SourceMessage(pos,
          safe"$pattern of type `${Types.toSource(patternType)}` can never match a value of type `${Types.toSource(fromType)}`"
        ))
      }
      else An.result(Types.narrow(fromType, patternType))
    case Some(fromType) => TypeInterpreter.expectAssignable(fromType, patternType, pos).map(_ => patternType)
  }

  // A literal pattern matches only some values of its type (there are no literal types), so it can fail to match
  // and is only allowed where failing is possible: the cases of #(), not assignments or function parameters.
  // TODO revisit if literal types get added
  def expectRefutable(pos: Pos): An[Unit] =
    if (refutable) An.result(())
    else An.error(SourceMessage(pos, safe"${quotedPattern(pos)} can fail to match, literal patterns can only be used in the cases of a piecewise function"))

  def quotedPattern(pos: Pos): String = pos.sourceText.fold("Pattern")(text => safe"Pattern `$text`")

}

object PatternAnalyzer {
  def walkAssignment(env: Env, targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] =
    PatternAnalyzerImpl(env, refutable = false).walk(targetPattern, fromType)

  def walkCase(env: Env, casePattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] =
    PatternAnalyzerImpl(env, refutable = true).walk(casePattern, fromType)
}
