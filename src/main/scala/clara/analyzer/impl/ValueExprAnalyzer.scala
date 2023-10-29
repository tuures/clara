package clara.analyzer.impl

import clara.asg.{Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class ValueExprAnalyzerImpl(env: Env) {
  def useNullaryType(name: String, pos: Pos): An[Types.Type] = env.useTypeCon(name, pos).flatMap { typeCon =>
    TypeInterpreter.instantiate(typeCon, Nil, pos)
  }

  def valueExprTerm(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => useNullaryType("Int", pos).map { typ =>
      Terms.IntegerLiteral(value, typ)
    }
    case Ast.FloatLiteral(value, pos) => useNullaryType("Float", pos).map { typ =>
      Terms.FloatLiteral(value, typ)
    }
    case Ast.StringLiteral(parts, pos) => useNullaryType("String", pos).map { typ =>
      Terms.StringLiteral(parts, typ)
    }
    case _: Ast.Tuple => ???
    case b: Ast.Block => BlockAnalyzer.blockTerm(env, b)
    case Ast.NamedValue(name, pos) => env.useValue(name, pos).map(typ => Terms.NamedValue(name, typ))
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
          case Types.Func(parameterType, resultType) =>
            ((parameterType, argument) match {
              case (Types.Func(p, _), l: Ast.Lambda) => lambdaTerm(l, Some(p))
              case _ => valueExprTerm(argument)
            }).flatMap { argumentTerm =>
              TypeInterpreter.expectAssignable(argumentTerm.typ, parameterType, argument.pos).map { case () =>
                Terms.Call(calleeTerm, argumentTerm, resultType)
              }
            }
          case _ =>
            An.error(SourceMessage(callee.pos, safe"Cannot call value of type `${Types.toSource(calleeTerm.typ)}`"))
        }
      }
    }
  }

  def lambdaTerm(lambda: Ast.Lambda, expectedParameterType: Option[Types.Type]): An[Terms.Lambda] = {
    val Ast.Lambda(typeParams, parameter, body, _) = lambda

    TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withParamsEnv, typeParamCons) =>
        PatternAnalyzer(withParamsEnv, withParamsEnv).walkAssignment(parameter, expectedParameterType).
        flatMap { case (funcBodyEnv, parameterTerm) =>
          ValueExprAnalyzerImpl(funcBodyEnv).valueExprTerm(body).map { bodyTerm =>
            val typ: Types.Type = typeParamCons match {
              // FIXME ugly
              case Nil => Types.Func(parameterTerm.typ, bodyTerm.typ)
              case _ => Types.PolyFunc(typeParamCons, parameterTerm.typ, bodyTerm.typ)
            }
            Terms.Lambda(parameterTerm, bodyTerm, typ)
          }
        }
    }
  }
}

object ValueExprAnalyzer {
  def valueExprTerm(env: Env, valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] =
    ValueExprAnalyzerImpl(env).valueExprTerm(valueExpr)
}
