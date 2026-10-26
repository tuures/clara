package clara.analyzer.impl

import clara.asg.{TermLiteral, Terms, Types}
import clara.ast.{Ast, AstLiteral, Pos, SourceMessage}

import clara.util.Safe._

// refutable: whether the pattern only has to be able to match some values (a case of #()) instead of every value
// (a value def or a function parameter), see matchType. Applies to the whole pattern including nested patterns.
// declared: whether the names the pattern binds were declared by the names pass of the scope (a value def), so that
// the pattern defines them, instead of adding new names (a parameter, a case).
case class PatternAnalyzerImpl(env: Env, refutable: Boolean, declared: Boolean) {
  def walk(targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] = targetPattern match {
    case Ast.WildcardPattern(pos) =>
      val typ = fromType.getOrElse(Types.Top)
      An.valid((env, Terms.WildcardPattern(typ)))
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
        case None => An.valid(ps.map(_ => None))
        case Some(fromType) => matchType(Some(fromType), Types.Tuple(ps.map(_ => Types.Top)), pos).flatMap { _ =>
          TypeInterpreter.tupleElementTypes(fromType, ps.length).map(_.map(Some(_)))
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
    case Ast.NamedConstantPattern(nsPath, name, pos) => namedConstant(nsPath, name, fromType, pos)
    case Ast.CapturePattern(name, pos) => capture(name, fromType, pos)
    case Ast.GuardPattern(p, guard, pos) => ???
    case Ast.DefaultValuePattern(p, default, pos) => ???
    case Ast.PatternAs(p, t, pos) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { targetType =>
        // a case quotes the whole pattern, an assignment points at the type annotation the value doesn't fit
        matchType(fromType, targetType, if (refutable) pos else t.pos).flatMap { typ =>
          walk(p, Some(typ)).map { case (nextEnv, patternTerm) =>
            (nextEnv, Terms.PatternAs(patternTerm, typ))
          }
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
          An.valid((currentEnv, currentParts :+ TermLiteral.StringPlainPart(value)))
        case AstLiteral.StringEscapePart(escapes) =>
          An.valid((currentEnv, currentParts :+ TermLiteral.StringEscapePart(escapes)))
        case AstLiteral.StringNamedConstantPart(Ast.NamedConstantPattern(nsPath, name, pos)) =>
          copy(env = currentEnv).namedConstant(nsPath, name, Some(typ), pos).map { case (nextEnv, pattern) =>
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

  def namedConstant(
    nsPath: Seq[Ast.NameWithPos],
    name: Ast.NameWithPos,
    fromType: Option[Types.Type],
    pos: Pos,
  ): An[(Env, Terms.NamedConstantPattern)] =
    NameResolver.value(env, nsPath, name.name, name.pos).flatMap { term =>
      matchType(fromType, term.typ, pos).map { _ =>
        (env, Terms.NamedConstantPattern(term))
      }
    }

  def capture(name: String, fromType: Option[Types.Type], pos: Pos): An[(Env, Terms.CapturePattern)] =
    // FIXME default to Top type and just give warning?
    An.fromSomeOrError(fromType, SourceMessage(pos, "Could not infer type")).flatMap { fromType =>
      (if (declared) env.defineValue(name, fromType, pos) else env.addValue(name, fromType, pos)).map { nextEnv =>
        (nextEnv, Terms.CapturePattern(name, fromType))
      }
    }

  // Checks that a pattern matching values of patternType can match a value of fromType and returns the type of the
  // matched value. An irrefutable pattern has to match every value of fromType, its type stays patternType.
  // A refutable pattern only has to be able to match some value of fromType and the type narrows to the values it
  // matches, e.g. 1 on Int | String narrows to Int.
  def matchType(fromType: Option[Types.Type], patternType: Types.Type, pos: Pos): An[Types.Type] = fromType match {
    case None => An.valid(patternType)
    case Some(fromType) if refutable => TypeInterpreter.disjoint(fromType, patternType).flatMap {
      case true =>
        val pattern = PatternAnalyzer.quotedPattern(pos)
        An.error(SourceMessage(pos,
          safe"$pattern of type `${Types.toSource(patternType)}` can never match a value of type `${Types.toSource(fromType)}`"
        ))
      case false => TypeInterpreter.narrow(fromType, patternType)
    }
    case Some(fromType) => TypeInterpreter.expectAssignable(fromType, patternType, pos).map(_ => patternType)
  }

  // A literal pattern matches only some values of its type (there are no literal types), so it can fail to match
  // and is only allowed where failing is possible: the cases of #(), not assignments or function parameters.
  // TODO revisit if literal types get added
  def expectRefutable(pos: Pos): An[Unit] =
    if (refutable) An.valid(())
    else An.error(SourceMessage(pos, safe"${PatternAnalyzer.quotedPattern(pos)} can fail to match, literal patterns can only be used in the cases of a piecewise function"))

}

object PatternAnalyzer {
  def walkValueDef(env: Env, targetPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] =
    PatternAnalyzerImpl(env, refutable = false, declared = true).walk(targetPattern, fromType)

  def walkParameter(env: Env, parameterPattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] =
    PatternAnalyzerImpl(env, refutable = false, declared = false).walk(parameterPattern, fromType)

  def walkCase(env: Env, casePattern: Ast.Pattern, fromType: Option[Types.Type]): An[(Env, Terms.Pattern)] =
    PatternAnalyzerImpl(env, refutable = true, declared = false).walk(casePattern, fromType)

  // The type whose every value the pattern matches, e.g. 1 has type Int but doesn't cover it
  def coveredType(pattern: Terms.Pattern): Types.Type = pattern match {
    case Terms.WildcardPattern(typ) => typ
    case Terms.UnitPattern() => Types.Uni
    case _: Terms.IntegerPattern | _: Terms.FloatPattern => Types.Bottom
    // a single capture matches every string
    case Terms.StringPattern(Seq(_: TermLiteral.StringCapturePart), typ) => typ
    case _: Terms.StringPattern => Types.Bottom
    case Terms.TuplePattern(ps, _) => ps.map(coveredType) match {
      case covered if covered.contains(Types.Bottom) => Types.Bottom
      case covered => Types.Tuple(covered)
    }
    case Terms.NamedConstantPattern(term) => term.typ match {
      // types with a single value
      case typ @ (_: Types.Singleton | Types.Uni) => typ
      case _ => Types.Bottom
    }
    case Terms.CapturePattern(_, typ) => typ
    case Terms.PatternAs(p, _) => coveredType(p)
  }

  def boundNames(pattern: Ast.Pattern): Seq[Ast.NameWithPos] = pattern match {
    case _: Ast.WildcardPattern | _: Ast.UnitPattern | _: Ast.IntegerPattern | _: Ast.FloatPattern => Nil
    case Ast.StringPattern(parts, _) => parts.collect {
      case AstLiteral.StringCapturePart(Ast.CapturePattern(name, pos)) => Ast.NameWithPos(name, pos)
    }
    case Ast.TuplePattern(ps, _) => ps.flatMap(boundNames)
    case _: Ast.NamedConstantPattern => Nil
    case Ast.CapturePattern(name, pos) => Seq(Ast.NameWithPos(name, pos))
    case Ast.GuardPattern(p, _, _) => p.toSeq.flatMap(boundNames)
    case Ast.DefaultValuePattern(p, _, _) => boundNames(p)
    case Ast.PatternAs(p, _, _) => boundNames(p)
    case Ast.RecordPattern(fields, _) => fields.flatMap { field =>
      field.subPattern.fold(Seq(Ast.NameWithPos(field.name, field.pos)))(boundNames)
    }
    // TODO: every alternative binds the same names?
    case Ast.OrPattern(ps, _) => ??? // ps.headOption.toSeq.flatMap(boundNames)
    case Ast.ConstructPattern(_, argumentPattern, _) => boundNames(argumentPattern)
  }

  def quotedPattern(pos: Pos): String = pos.sourceText.fold("Pattern")(text => safe"Pattern `$text`")
}
