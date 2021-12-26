package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._
import clara.asg.Types.Alias


case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => env.useTypeInst("Int", Nil, pos).map { typ =>
      Terms.IntegerLiteral(value, typ)
    }
    case Ast.FloatLiteral(value, pos) => env.useTypeInst("Float", Nil, pos).map { typ =>
      Terms.FloatLiteral(value, typ)
    }
    case Ast.StringLiteral(parts, pos) => env.useTypeInst("String", Nil, pos).map { typ =>
      Terms.StringLiteral(parts, typ)
    }
    case _: Ast.Tuple => ???
    case Ast.Block(bcs, pos) => BlockAnalyzer(env).walkBlock(bcs, pos)
    case Ast.NamedValue(name, pos) => env.useValue(name, pos).map(typ => Terms.NamedValue(name, typ))
    case Ast.ValueAs(e, t, pos) =>
      walkValueExpr(e).zip(TypeExprAnalyzer(env).walkTypeExpr(t)).flatMap { case (term, typ) =>
        TypeAnalyzer.expectAssignable(term.typ, typ, pos).map((_: Unit) => term)
      }
    case Ast.Record(fields, _) => {
      An.step(fields)(Namespace.empty[Terms.Field]){ case (ns, Ast.FieldDef(name, typeOpt, body, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        typeOpt.foreach(_ => ???) // FIXME

        walkValueExpr(body).flatMap { bodyTerm =>
          An.someOrError(ns.add((name, Terms.Field(bodyTerm))), duplicateName)
        }
      }.map { fields =>
        Terms.Record(fields, Types.Record(fields.mapValues(_.body.typ)))
      }
    }
    case Ast.Lambda(parameter, body, pos) => ???
    case Ast.MemberSelection(obj, Ast.NamedMember(name/*, typeArgs*/, memberPos), pos) =>
      walkValueExpr(obj).flatMap { objectTerm =>
        MemberSelectionAnalyzer(env, name, memberPos).walkMemberSelection(objectTerm).map { case (selectedMember, typ) =>
          Terms.MemberSelection(objectTerm, name, selectedMember, typ)
        }
      }
    case Ast.Call(callee, argument, pos) =>
      walkValueExpr(callee).zip(walkValueExpr(argument)).flatMap { case (calleeTerm, argumentTerm) =>
        calleeTerm.typ match {
          case Types.Func(parameterType, resultType) =>
            TypeAnalyzer.expectAssignable(argumentTerm.typ, parameterType, argument.pos).map { _: Unit =>
              Terms.Call(calleeTerm, argumentTerm, resultType)
            }
          case _ => An.error(SourceMessage(callee.pos, safe"Cannot call type `${Types.toSource(calleeTerm.typ)}`"))
        }
      }
    case Ast.NewExpr(namedType, _) => walkNewExpr(namedType)
  }

  def walkNewExpr(namedType: Ast.NamedType): An[Terms.NewExpr] = {
    def walkNew(typ: Types.Type): An[Terms.NewExpr] = typ match {
      case Alias(_, wrappedType) => walkNew(wrappedType)
      case typ @ Types.Unique(constructible, wrappedType, _) if constructible =>
        An.result(Terms.NewExpr(Types.Func(wrappedType, typ)))
      case typ =>
        An.error(SourceMessage(namedType.pos, safe"Cannot construct new values of type `${Types.toSource(typ)}`"))
    }

    TypeExprAnalyzer(env).walkTypeExpr(namedType).flatMap(walkNew)
  }

}
