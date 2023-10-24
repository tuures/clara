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
      An.step(fields)(Namespace.empty[Terms.Field]){ case (ns, Ast.FieldDef(name, typeOpt, body, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        typeOpt.foreach(_ => ???) // FIXME

        valueExprTerm(body).flatMap { bodyTerm =>
          An.fromSomeOrError(ns.add((name, Terms.Field(bodyTerm))), duplicateName)
        }
      }.map { fields =>
        Terms.Record(fields, Types.Record(fields.mapValues(_.body.typ)))
      }
    }
    case Ast.Lambda(parameter, body, pos) => ???
    case Ast.MemberSelection(obj, Ast.NamedMember(name/*, typeArgs*/, memberPos), pos) =>
      valueExprTerm(obj).flatMap { objectTerm =>
        MemberSelectionAnalyzer(env, name, memberPos).walkMemberSelection(objectTerm).map { case (selectedMember, typ) =>
          Terms.MemberSelection(objectTerm, name, selectedMember, typ)
        }
      }
    case Ast.Call(callee, argument, pos) =>
      valueExprTerm(callee).zip(valueExprTerm(argument)).flatMap { case (calleeTerm, argumentTerm) =>
        calleeTerm.typ match {
          case Types.Func(parameterType, resultType) =>
            TypeInterpreter.expectAssignable(argumentTerm.typ, parameterType, argument.pos).map { case () =>
              Terms.Call(calleeTerm, argumentTerm, resultType)
            }
          case _ =>
            An.error(SourceMessage(callee.pos, safe"Cannot call value of type `${Types.toSource(calleeTerm.typ)}`"))
        }
      }
  }
}

object ValueExprAnalyzer {
  def valueExprTerm(env: Env, valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] =
    ValueExprAnalyzerImpl(env).valueExprTerm(valueExpr)
}
