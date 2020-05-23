package clara.analyzer.impl

import clara.asg.{Types, Namespace}
import clara.ast.{Ast, SourceMessage}

import ai.x.safe._


case class TypeExprAnalyzer(env: Env) {
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Types.MonoType] = typeExpr match {
    case Ast.TopType(_) => An.result(Types.Top)
    case Ast.BottomType(_) => An.result(Types.Bottom)
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.TupleType(ts, pos) => ???
    case Ast.NamedType(name, typeArgs, pos) =>
      An.seq(typeArgs.map(walkTypeExpr)).flatMap { args =>
        env.useType(name, args, pos)
      }
    case Ast.RecordType(fields, _) =>
      An.step(fields)(Namespace.empty[Types.MonoType]){ case (ns, Ast.FieldDecl(name, typeExpr, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        walkTypeExpr(typeExpr).flatMap { typ =>
          An.someOrError(ns.add((name, typ)), duplicateName)
        }
      }.map(Types.Record(_))
    case Ast.FuncType(parameter, result, pos) => {
      val tea = TypeExprAnalyzer(env)
      tea.walkTypeExpr(parameter).zip(tea.walkTypeExpr(result)).map { case (parameterTyp, resultTyp) =>
        Types.Func(parameterTyp, resultTyp)
      }
    }
  }

}
