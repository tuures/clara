package clara.analyzer.impl

import clara.asg.{Namespace, TermLiteral, Terms, Types}
import clara.ast.{Ast, AstLiteral, Pos, SourceMessage}

import clara.util.Safe._


case class ValueExprAnalyzerImpl(env: Env) {
  def namedNullaryType(name: String, pos: Pos): An[Types.Type] =
    TypeExprAnalyzer.namedNullaryType(env, name, pos)

  def namedValue(name: String, pos: Pos): An[Terms.NamedValue] = NameResolver.value(env, Nil, name, pos)

  def lambdaTerm(lambda: Ast.Lambda, expectedParameterType: Option[Types.Type]): An[Terms.Lambda] = {
    val Ast.Lambda(typeParams, parameter, body, _) = lambda

    TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withTypeParamsEnv, typeParamCons) =>
      PatternAnalyzer.walkParameter(withTypeParamsEnv, parameter, expectedParameterType).
        flatMap { case (funcBodyEnv, parameterTerm) =>
          ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
            val typ = Types.Func(typeParamCons, parameterTerm.typ, bodyTerm.typ)
            Terms.Lambda(parameterTerm, bodyTerm, typ)
          }
        }
      }
  }

  def piecewiseTerm(piecewise: Ast.Piecewise, expectedParameterType: Option[Types.Type]): An[Terms.Piecewise] = {
    case class PieceState(pieces: Seq[((Terms.Pattern, Terms.ValueExpr), Types.Func)])
    An.step(piecewise.pieces)(PieceState(Nil)) { case (state, (pattern, body)) =>

      PatternAnalyzer.walkCase(env.startNestedScope, pattern, expectedParameterType).
        flatMap { case (funcBodyEnv, parameterTerm) =>
          ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
            val typ = Types.Func(Nil, parameterTerm.typ, bodyTerm.typ)

            state.copy(pieces = state.pieces ++ Seq(((parameterTerm, bodyTerm), typ)))
          }
        }

    }.flatMap { state =>
      val (patternsWithBodies, funcTypes) = state.pieces.unzip
      val typ = Types.Intersection(funcTypes)

      TypeInterpreter.casesCheck(piecewise, patternsWithBodies.map(_._1), expectedParameterType).map { case () =>
        Terms.Piecewise(patternsWithBodies, typ)
      }
    }
  }

  def callTerm(callee: Ast.ValueExpr, argument: Ast.ValueExpr): An[Terms.Call] = callee match {
    // for a literal lambda or piecewise callee, resolve the argument first and use its type as the expected parameter
    // type, e.g. in x @ #(...) the cases know the type of x
    case lambda: Ast.Lambda => argumentFirstCallTerm(callee, argument)(lambdaTerm(lambda, _))
    case piecewise: Ast.Piecewise => argumentFirstCallTerm(callee, argument)(piecewiseTerm(piecewise, _))
    // ... otherwise resolve callee first as it should have type already and that will help resolve the argument (e.g. when literal lambda or piecewise is passed as an argument to a higher order function which is being called)
    case _ => calleeFirstCallTerm(callee, argument)
  }

  def argumentFirstCallTerm(callee: Ast.ValueExpr, argument: Ast.ValueExpr)(
    calleeTerm: Option[Types.Type] => An[Terms.ValueExpr]
  ): An[Terms.Call] =
    valueExprTerm(argument).flatMap { argumentTerm =>
      calleeTerm(Some(argumentTerm.typ)).flatMap { calleeTerm =>
        callResultType(calleeTerm, callee.pos, argumentTerm, argument.pos).map { resultType =>
          Terms.Call(calleeTerm, argumentTerm, resultType)
        }
      }
    }

  def calleeFirstCallTerm(callee: Ast.ValueExpr, argument: Ast.ValueExpr): An[Terms.Call] =
    valueExprTerm(callee).flatMap { calleeTerm =>
      // the callee's parameter type helps to infer types of literal lambdas and piecewise functions in the argument
      val expectedArgumentType = calleeTerm.typ match {
        case f: Types.Func => Some(f.parameter)
        case _ => None
      }

      valueExprTermExpecting(argument, expectedArgumentType).flatMap { argumentTerm =>
        callResultType(calleeTerm, callee.pos, argumentTerm, argument.pos).map { resultType =>
          Terms.Call(calleeTerm, argumentTerm, resultType)
        }
      }
    }

  def callResultType(calleeTerm: Terms.ValueExpr, calleePos: Pos, argumentTerm: Terms.ValueExpr, argumentPos: Pos): An[Types.Type] = {
    lazy val cannotCall =
      An.error(SourceMessage(calleePos, safe"Cannot call value of type `${Types.toSource(calleeTerm.typ)}`"))

    calleeTerm.typ match {
      case f: Types.Func => funcCallResultType(f, argumentTerm, argumentPos)
      case Types.Intersection(ts) => ts.flatMap {
        // TODO: is it ok that we just discard the intersection type members that are polymorphic functions. Check if parser already rejects them, if not we should probably give an error here instead of just ignoring them.
        case f @ Types.Func(Nil, _, _) => Some(f)
        case _ => None
      } match {
        case Nil => cannotCall
        case funcs => intersectionCallResultType(funcs, argumentTerm, argumentPos)
      }
      // TODO make object callable if it has apply method?
      case _ => cannotCall
    }
  }

  def funcCallResultType(calleeFunc: Types.Func, argumentTerm: Terms.ValueExpr, argumentPos: Pos): An[Types.Type] = {
    val Types.Func(_, inferredParameterType, inferredResultType) = calleeFunc.typeParams match {
      case Nil => calleeFunc
      case _ => inferFuncTypeArgs(calleeFunc, argumentTerm.typ)
    }

    TypeInterpreter.expectAssignable(argumentTerm.typ, inferredParameterType, argumentPos).map { case () =>
      inferredResultType
    }
  }

  def intersectionCallResultType(calleeFuncs: Seq[Types.Func], argumentTerm: Terms.ValueExpr, argumentPos: Pos): An[Types.Type] = {
    val parameterUnion = Types.Union(calleeFuncs.map(_.parameter))

    // TODO: narrow down the resultType based on which branches the argument is compatible with.
    TypeInterpreter.expectAssignable(argumentTerm.typ, parameterUnion, argumentPos).map { case () =>
      Types.Union(calleeFuncs.map(_.result))
    }
  }

  def inferFuncTypeArgs(polyFunc: Types.Func, argumentType: Types.Type): Types.Func = {
    val Types.Func(typeParams, parameterType, resultType) = polyFunc

    val foundSubstitutions = Types.findSubstitutions(typeParams.map(_.uniq).toSet, parameterType, argumentType)

    val inferredTypeArgs = typeParams.map { paramCon =>
      // FIXME are there any "valid" cases where param cannot be resolved other than unused type parameter?
      (paramCon.uniq, foundSubstitutions.getOrElse(paramCon.uniq, Types.Bottom))
    }.toMap

    val substituteParams = Types.substituteParams(inferredTypeArgs, _)

    Types.Func(Nil, substituteParams(parameterType), substituteParams(resultType))
  }

  def valueExprTermExpecting(valueExpr: Ast.ValueExpr, expectedType: Option[Types.Type]): An[Terms.ValueExpr] =
    (valueExpr, expectedType) match {
      case (_, Some(alias: Types.Alias)) =>
        TypeInterpreter.wrappedType(alias).flatMap(wrappedType => valueExprTermExpecting(valueExpr, Some(wrappedType)))
      case (l: Ast.Lambda, Some(Types.Func(Nil, p, _))) => lambdaTerm(l, Some(p))
      case (pw: Ast.Piecewise, Some(Types.Func(Nil, p, _))) => piecewiseTerm(pw, Some(p))
      case (Ast.Tuple(es, _), Some(Types.Tuple(expectedTypes))) if es.length == expectedTypes.length =>
        tupleTerm(es, expectedTypes.map(Some(_)))
      case (Ast.Record(fields, _), Some(Types.Record(expectedFieldTypes))) => recordTerm(fields, expectedFieldTypes)
      case _ => valueExprTerm(valueExpr)
    }

  def tupleTerm(elements: Seq[Ast.ValueExpr], expectedTypes: Seq[Option[Types.Type]]): An[Terms.Tuple] =
    An.seq(elements.zip(expectedTypes).map { case (e, expectedType) => valueExprTermExpecting(e, expectedType) }).map { terms =>
      Terms.Tuple(terms, Types.Tuple(terms.map(_.typ)))
    }

  def recordTerm(fields: Seq[Ast.FieldDef], expectedFieldTypes: Namespace[Types.Type]): An[Terms.Record] =
    An.step(fields)(Namespace.empty[Terms.Field]){ case (ns, Ast.FieldDef(name, typeExprOpt, body, pos)) =>
      lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

      typeExprOpt.fold(An.valid(Option.empty[(Types.Type, Pos)])) { typeExpr =>
        TypeExprAnalyzer.typeExprType(env, typeExpr).map(typ => Some((typ, typeExpr.pos)))
      }.flatMap { annotation =>
        // the field's own type annotation takes precedence over the expected type
        val expectedType = annotation.map(_._1).orElse(expectedFieldTypes.get(name))

        valueExprTermExpecting(body, expectedType).flatMap { bodyTerm =>
          annotation.fold(An.valid(())) { case (typ, typePos) =>
            TypeInterpreter.expectAssignable(bodyTerm.typ, typ, typePos)
          }.flatMap { case () =>
            An.fromSomeOrError(ns.add((name, Terms.Field(bodyTerm))), duplicateName)
          }
        }
      }
    }.map { fields =>
      Terms.Record(fields, Types.Record(fields.mapValues(_.body.typ)))
    }

  def memberSelection(obj: Ast.ValueExpr, name: String, memberPos: Pos): An[Terms.ValueExpr] =
    NameResolver.valueNsPath(env, obj) match {
      // obj is a named namespace, resolve name there
      case Some(nsPath) => NameResolver.value(env, nsPath, name, memberPos)
      // otherwise obj must be a value, try to resolve the member from it
      case None => valueExprTerm(obj).flatMap { objectTerm =>
        MemberSelectionAnalyzer.memberSelection(env, objectTerm, name, memberPos).map { case (selectedMember, typ) =>
          Terms.MemberSelection(objectTerm, name, selectedMember, typ)
        }
      }
    }

  def valueExprTerm(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.valid(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => namedNullaryType("Int", pos).map { typ =>
      Terms.IntegerLiteral(TermLiteral.integer(value), typ)
    }
    case Ast.FloatLiteral(AstLiteral.Float(whole, fraction), pos) => namedNullaryType("Float", pos).map { typ =>
      Terms.FloatLiteral(TermLiteral.Float(whole, fraction), typ)
    }
    case Ast.StringLiteral(parts, pos) =>
      val analyzedParts: An[Seq[TermLiteral.StringValuePart]] = An.seq(parts.map {
        case AstLiteral.StringPlainPart(value) => An.valid(TermLiteral.StringPlainPart(value))
        case AstLiteral.StringEscapePart(escapes) => An.valid(TermLiteral.StringEscapePart(escapes))
        case AstLiteral.StringValueExprPart(e) => valueExprTerm(e).map(TermLiteral.StringValueExprPart(_))
      })
      analyzedParts.zip(namedNullaryType("String", pos)).map { case (parts, typ) =>
        Terms.StringLiteral(parts, typ)
      }
    case Ast.Tuple(es, _) => tupleTerm(es, es.map(_ => None))
    case b: Ast.Block => BlockAnalyzer.regularBlockTerm(env, b)
    case Ast.NamedValue(name, pos) => namedValue(name, pos)
    case Ast.ValueAs(e, t, _) =>
      TypeExprAnalyzer.typeExprType(env, t).flatMap { typ =>
        valueExprTermExpecting(e, Some(typ)).flatMap { term =>
          TypeInterpreter.expectAssignable(term.typ, typ, t.pos).map { case () =>
            Terms.ValueAs(term, typ)
          }
        }
      }
    case Ast.Record(fields, _) => recordTerm(fields, Namespace.empty)
    case l: Ast.Lambda => lambdaTerm(l, None)
    case p: Ast.Piecewise => piecewiseTerm(p, None)
    // FIXME Ast.NamedMember?
    case Ast.MemberSelection(obj, Ast.NamedValue(name, memberPos), _) => memberSelection(obj, name, memberPos)
    case Ast.Call(callee, argument, _) => callTerm(callee, argument)
    case Ast.Pipe(argument, callee, _) => callTerm(callee, argument)
  }
}

object ValueExprAnalyzer {
  def namedValue(env: Env, name: String, pos: Pos): An[Terms.NamedValue] =
    ValueExprAnalyzerImpl(env).namedValue(name, pos)
  def valueExprTerm(env: Env, valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] =
    ValueExprAnalyzerImpl(env).valueExprTerm(valueExpr)
}
