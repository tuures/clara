package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class ValueExprAnalyzerImpl(env: Env) {
  def namedNullaryType(name: String, pos: Pos): An[Types.Type] =
    TypeExprAnalyzer.namedNullaryType(env, name, pos)

  def lambdaTerm(lambda: Ast.Lambda, expectedParameterType: Option[Types.Type]): An[Terms.Lambda] = {
    val Ast.Lambda(typeParams, parameter, body, _) = lambda

    TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withParamsEnv, typeParamCons) =>
        PatternAnalyzer(withParamsEnv, withParamsEnv).walkAssignment(parameter, expectedParameterType).
        flatMap { case (funcBodyEnv, parameterTerm) =>
          ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
            val typ = Types.Func(typeParamCons, parameterTerm.typ, bodyTerm.typ)
            Terms.Lambda(parameterTerm, bodyTerm, typ)
          }
        }
    }
  }

  def functionCallArgumentTerm(calleeParameter: Types.Type, argument: Ast.ValueExpr): An[Terms.ValueExpr] =
    (calleeParameter, argument) match {
      // for inline lambda, infer the lambda's parameter type from the callee's parameter type
      // avoiding the need for type annotations on the parameter pattern of the lambda
      case (Types.Func(Nil, p, _), l: Ast.Lambda) => lambdaTerm(l, Some(p))
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

  def functionCall(calleeFunc: Types.Func, argument: Ast.ValueExpr): An[(Terms.ValueExpr, Types.Type)] = {
    functionCallArgumentTerm(calleeFunc.parameter, argument).flatMap { argumentTerm =>
      val Types.Func(_, inferredParameterType, inferredResultType) = calleeFunc.typeParams match {
        case Nil => calleeFunc
        case _ => inferFuncTypeArgs(calleeFunc, argumentTerm.typ)
      }

      TypeInterpreter.expectAssignable(argumentTerm.typ, inferredParameterType, argument.pos).map { case () =>
        (argumentTerm, inferredResultType)
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
    case _: Ast.Tuple => ???
    case b: Ast.Block => BlockAnalyzer.blockTerm(env, b)
    case Ast.NamedValue(name, pos) => {
      env.values.get(name).map(typ => An.result(Terms.NamedValue(name, typ))).orElse {
        env.typeCons.get(name).map { con =>
          con match {
            case con: TypeCons.WrapperTypeCon =>
              An.result(Terms.NamedValue(name, TypeInterpreter.wrapperConstructorFunc(con)))
            case _ => An.error(SourceMessage(pos, safe"Unknown value `$name`. Type `$name` cannot be used as a value."))
          }
        }
      }.getOrElse(An.error(SourceMessage(pos, safe"Unknown value `$name`")))
    }
    case Ast.ValueAs(e, t, pos) =>
      valueExprTerm(e).zip(TypeExprAnalyzer.typeExprType(env, t)).flatMap { case (term, typ) =>
        TypeInterpreter.expectAssignable(term.typ, typ, pos).map((_: Unit) => term)
      }
    case Ast.Record(fields, _) => {
      An.step(fields)(Namespace.empty[Terms.Field]){ case (ns, Ast.FieldDef(name, typeExprOpt, body, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        valueExprTerm(body).flatMap { bodyTerm =>
          typeExprOpt.fold(An.result(())) { typExpr =>
            TypeExprAnalyzer.typeExprType(env, typExpr).flatMap { typ =>
              TypeInterpreter.expectAssignable(bodyTerm.typ, typ, pos)
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
    case Ast.MemberSelection(obj, Ast.NamedMember(name, memberPos), _) =>
      valueExprTerm(obj).flatMap { objectTerm =>
        MemberSelectionAnalyzer(env, name, memberPos).walkMemberSelection(objectTerm).map { case (selectedMember, typ) =>
          Terms.MemberSelection(objectTerm, name, selectedMember, typ)
        }
      }
    case Ast.Call(callee, argument, _) => {
      valueExprTerm(callee).flatMap { calleeTerm =>
        calleeTerm.typ match {
          case f: Types.Func =>
            functionCall(f, argument).map { case (argumentTerm, resultType) =>
              Terms.Call(calleeTerm, argumentTerm, resultType)
            }
          case _ =>
            An.error(SourceMessage(callee.pos, safe"Cannot call value of type `${Types.toSource(calleeTerm.typ)}`"))
        }
      }
    }
  }
}

object ValueExprAnalyzer {
  def valueExprTerm(env: Env, valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] =
    ValueExprAnalyzerImpl(env).valueExprTerm(valueExpr)
}
