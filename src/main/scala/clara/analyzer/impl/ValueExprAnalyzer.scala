package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._
import clara.asg.Types.Alias


case class ValueExprAnalyzer(env: Env) {
  def walkValueExpr(valueExpr: Ast.ValueExpr): An[Terms.ValueExpr] = valueExpr match {
    case Ast.UnitLiteral(_) => An.result(Terms.UnitLiteral())
    case Ast.IntegerLiteral(value, pos) => env.useType("Int", Nil, pos).map { typ =>
      Terms.IntegerLiteral(value, typ)
    }
    case Ast.FloatLiteral(value, pos) => env.useType("Float", Nil, pos).map { typ =>
      Terms.FloatLiteral(value, typ)
    }
    case Ast.StringLiteral(parts, pos) => env.useType("String", Nil, pos).map { typ =>
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
        walkMemberSelection(objectTerm, name, memberPos).map { case (selectedMember, typ) =>
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

  def walkMemberSelection(
    objectTerm: Terms.ValueExpr,
    name: String,
    memberPos: Pos
  ): An[(Terms.SelectedMember, Types.MonoType)] = {
    lazy val memberNotFound = SourceMessage(memberPos, safe"`$name` is not a member of type `${Types.toSource(objectTerm.typ)}`")

    def memberOfType(typ: Types.Typ): Option[(Terms.SelectedMember, Types.MonoType)] = (env.methods.get(typ).flatMap {
      case declSection: Terms.MethodDeclSection => declSection.methodDecls.get(name).map(m => (m.attributes, m.typ))
      case defSection: Terms.MethodDefSection => defSection.methodDefs.get(name).map(m => (m.attributes, m.body.typ))
    }).map { case (attributes, typ) =>
      (Terms.SelectedMethod(attributes), typ)
    }.orElse {
      typ match {
        case Types.Record(fields) => fields.get(name).map(typ => (Terms.SelectedField, typ))
        case Alias(_, wrappedType) => memberOfType(wrappedType)
        case Types.Unique(_, wrappedType, _) => memberOfType(wrappedType)
        case _ => None
      }
    }

    An.someOrError(memberOfType(objectTerm.typ), memberNotFound)
  }

  def walkNewExpr(namedType: Ast.NamedType): An[Terms.NewExpr] = {
    def walkNew(typ: Types.Typ): An[Terms.NewExpr] = typ match {
      case Alias(_, wrappedType) => walkNew(wrappedType)
      case typ @ Types.Unique(constructible, wrappedType, _) if constructible =>
        An.result(Terms.NewExpr(Types.Func(wrappedType, typ)))
      case typ =>
        An.error(SourceMessage(namedType.pos, safe"Cannot construct new values of type `${Types.toSource(typ)}`"))
    }

    TypeExprAnalyzer(env).walkTypeExpr(namedType).flatMap(walkNew)
  }

}
