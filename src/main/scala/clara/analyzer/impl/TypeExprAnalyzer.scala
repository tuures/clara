package clara.analyzer.impl

import clara.asg.{Types, Namespace}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._
import clara.ast.Ast.NamedType


case class TypeExprAnalyzer(env: Env) {
  //FIXME rename to walkMonoTypeExpr
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Types.Type] = typeExpr match {
    case Ast.TopType(_) => An.result(Types.Top)
    case Ast.BottomType(_) => An.result(Types.Bottom)
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.TupleType(ts, pos) => ???
    // case Ast.NamedType(name, typeArgs, pos) =>
    //   An.seq(typeArgs.map(walkTypeExpr)).flatMap { args =>
    //     env.useTypeInst(name, args, pos)
    //   }
    case Ast.RecordType(fields, _) =>
      An.step(fields)(Namespace.empty[Types.Type]){ case (ns, Ast.FieldDecl(name, typeExpr, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        walkTypeExpr(typeExpr).flatMap { typ =>
          An.fromSomeOrError(ns.add((name, typ)), duplicateName)
        }
      }.map(Types.Record(_))
    case Ast.FuncType(parameter, result, pos) => {
      walkTypeExpr(parameter).zip(walkTypeExpr(result)).map { case (parameterTyp, resultTyp) =>
        Types.Func(parameterTyp, resultTyp)
      }
    }
  }

  // def walkNamedUnique(typeExpr: Ast.TypeExpr): An[Types.Unique] = typeExpr match {
  //   case NamedType(name, Nil, pos) =>
  //   case _ => An.error(SourceMessage(typeExpr.pos, "Named type expected"))
  // }
}
