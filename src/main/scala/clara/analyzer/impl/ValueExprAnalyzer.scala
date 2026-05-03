package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class ValueExprAnalyzerImpl(env: Env) {
  def namedNullaryType(name: String, pos: Pos): An[Types.Type] =
    TypeExprAnalyzer.namedNullaryType(env, name, pos)

  def namedValue(name: String, pos: Pos): An[Terms.NamedValue] = {
    env.values.get(name).map(typ => An.result(Terms.NamedValue(name, typ))).orElse {
      env.typeCons.get(name).map { con =>
        con match {
          case con: TypeCons.WrapperTypeCon =>
            An.result(Terms.NamedValue(name, TypeCons.wrapperConstructorFunc(con)))
          case _ => An.error(SourceMessage(pos, safe"Unknown value `$name`. Type `$name` cannot be used as a value."))
        }
      }
    }.getOrElse(An.error(SourceMessage(pos, safe"Unknown value `$name`")))
  }

  def lambdaTerm(lambda: Ast.Lambda, expectedParameterType: Option[Types.Type]): An[Terms.Lambda] = {
    val Ast.Lambda(typeParams, parameter, body, _) = lambda

    TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withTypeParamsEnv, typeParamCons) =>
      PatternAnalyzer(withTypeParamsEnv, withTypeParamsEnv).walkAssignment(parameter, expectedParameterType).
        flatMap { case (funcBodyEnv, parameterTerm) =>
          ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
            val typ = Types.Func(typeParamCons, parameterTerm.typ, bodyTerm.typ)
            Terms.Lambda(parameterTerm, bodyTerm, typ)
          }
        }
      }
  }

  def funcCallArgumentTerm(calleeParameter: Types.Type, argument: Ast.ValueExpr): An[Terms.ValueExpr] =
    (calleeParameter, argument) match {
      // for lambda literal in call expression argument position, infer the lambda's parameter type from the callee's parameter type
      // avoiding the need for type annotations on the parameter pattern of the lambda
      case (Types.Func(Nil, p, _), l: Ast.Lambda) => lambdaTerm(l, Some(p))
      // TODO case (Type.Tuple, Ast.Tuple) with nested lambdas
      // TODO case (Type.Record, Ast.Record) with nested lambdas
      case _ => valueExprTerm(argument)
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

  def funcCall(calleeFunc: Types.Func, argument: Ast.ValueExpr): An[(Terms.ValueExpr, Types.Type)] = {
    funcCallArgumentTerm(calleeFunc.parameter, argument).flatMap { argumentTerm =>
      val Types.Func(_, inferredParameterType, inferredResultType) = calleeFunc.typeParams match {
        case Nil => calleeFunc
        case _ => inferFuncTypeArgs(calleeFunc, argumentTerm.typ)
      }

      TypeInterpreter.expectAssignable(argumentTerm.typ, inferredParameterType, argument.pos).map { case () =>
        (argumentTerm, inferredResultType)
      }
    }
  }

  def callTerm(callee: Ast.ValueExpr, argument: Ast.ValueExpr): An[Terms.Call] = callee match {
    // TODO if calleeTerm is a literal piecewise or lambda, resolve argument first and use that to help infer types in the piecewise
    case callee: Ast.Lambda => callTermX(callee, argument)
    case callee: Ast.Piecewise => callTermX(callee, argument)
    // ... otherwise resolve callee first as it should have type already and that will help resolve the argument (e.g. when literal lambda or piecewise is passed as an argument to a higher order function which is being called)
    case _ => callTermX(callee, argument)
  }

  def callTermX(callee: Ast.ValueExpr, argument: Ast.ValueExpr): An[Terms.Call] = {
    valueExprTerm(callee).flatMap { calleeTerm =>
      lazy val cannotCall =
        An.error(SourceMessage(callee.pos, safe"Cannot call value of type `${Types.toSource(calleeTerm.typ)}`"))

      calleeTerm.typ match {
        case f: Types.Func =>
          funcCall(f, argument).map { case (argumentTerm, resultType) =>
            Terms.Call(calleeTerm, argumentTerm, resultType)
          }
        case Types.Intersection(ts) => {
          valueExprTerm(argument).flatMap { argumentTerm =>
            ts.flatMap {
              // TODO: is it ok that we just discard the intersection type members that are polymorphic functions. Check if parser already rejects them, if not we should probably give an error here instead of just ignoring them.
              case f @ Types.Func(Nil, _, _) => Some(f)
              case _ => None
            } match {
              case Nil => cannotCall
              case funcs =>
                val parameterUnion = Types.Union(funcs.map(_.parameter))

                // TODO: narrow down the resultType based on which branches the argument is compatible with.
                TypeInterpreter.expectAssignable(argumentTerm.typ, parameterUnion, argument.pos).map { case () =>
                  val resultType = Types.Union(funcs.map(_.result))
                  Terms.Call(calleeTerm, argumentTerm, resultType)
                }
            }
          }
        }
        // TODO make object callable if it has apply method?
        case _ =>
          cannotCall
      }
    }
  }

  def valueExprTerm(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => namedNullaryType("Int", pos).map { typ =>
      Terms.IntegerLiteral(value, typ)
    }
    case Ast.FloatLiteral(value, pos) => namedNullaryType("Float", pos).map { typ =>
      Terms.FloatLiteral(value, typ)
    }
    case Ast.StringLiteral(parts, pos) => namedNullaryType("String", pos).map { typ =>
      Terms.StringLiteral(parts, typ)
    }
    case Ast.Tuple(es, _) => An.seq(es.map(valueExprTerm)).map { terms =>
      Terms.Tuple(terms, Types.Tuple(terms.map(_.typ)))
    }
    case b: Ast.Block => BlockAnalyzer.regularBlockTerm(env, b)
    case Ast.NamedValue(name, pos) => namedValue(name, pos)
    case Ast.ValueAs(e, t, _) =>
      // TODO: this does not actually change the type, just checks for assignability, is that ok?
      valueExprTerm(e).zip(TypeExprAnalyzer.typeExprType(env, t)).flatMap { case (term, typ) =>
        TypeInterpreter.expectAssignable(term.typ, typ, t.pos).map((_: Unit) => term)
      }
    case Ast.Record(fields, _) => {
      An.step(fields)(Namespace.empty[Terms.Field]){ case (ns, Ast.FieldDef(name, typeExprOpt, body, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        valueExprTerm(body).flatMap { bodyTerm =>
          typeExprOpt.fold(An.result(())) { typExpr =>
            TypeExprAnalyzer.typeExprType(env, typExpr).flatMap { typ =>
              TypeInterpreter.expectAssignable(bodyTerm.typ, typ, typExpr.pos)
            }
          }.flatMap { case () =>
            An.fromSomeOrError(ns.add((name, Terms.Field(bodyTerm))), duplicateName)
          }
        }
      }.map { fields =>
        Terms.Record(fields, Types.Record(fields.mapValues(_.body.typ)))
      }
    }
    case l: Ast.Lambda => lambdaTerm(l, None)
    case Ast.Piecewise(pieces, _) => {
      // TODO review and clean the code, extract?
      // TODO exhaustiveness check
      // TODO dead branch check
      case class PieceState(pieces: Seq[((Terms.Pattern, Terms.ValueExpr), Types.Func)])
      An.step(pieces)(PieceState(Nil)) { case (state, (pattern, body)) =>

        PatternAnalyzer(env, env).walkAssignment(pattern, None).
          flatMap { case (funcBodyEnv, parameterTerm) =>
            ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
              val typ = Types.Func(Nil, parameterTerm.typ, bodyTerm.typ)

              state.copy(pieces = state.pieces ++ Seq(((parameterTerm, bodyTerm), typ)))
            }
          }

      }.map { state =>
        val (patternsWithBodies, funcTypes) = state.pieces.unzip
        val typ = Types.Intersection(funcTypes)

        Terms.Piecewise(patternsWithBodies, typ)
      }
    }
    // FIXME Ast.NamedMember?
    case Ast.MemberSelection(obj, Ast.NamedValue(name, memberPos), _) =>
      valueExprTerm(obj).flatMap { objectTerm =>
        MemberSelectionAnalyzer.memberSelection(env, objectTerm, name, memberPos).map { case (selectedMember, typ) =>
          Terms.MemberSelection(objectTerm, name, selectedMember, typ)
        }
      }
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
