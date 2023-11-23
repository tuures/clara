package clara.analyzer.impl

import clara.asg.{Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._
import clara.asg.TypeCons

case class TypeExprAnalyzerImpl(env: Env) {
  def namedTypeCon(n: Ast.NameWithPos): An[TypeCons.TypeCon] =
    An.fromSomeOrError(env.typeCons.get(n.name), SourceMessage(n.pos, safe"Unknown type `${n.name}`"))

  def namedType(n: Ast.NameWithPos, typeArgs: Seq[Ast.TypeExpr], pos: Pos): An[Types.Type] =
    namedTypeCon(n).zip(An.seq(typeArgs.map(typeExprType))).flatMap { case (typeCon, args) =>
      TypeInterpreter.instantiate(typeCon, args, pos)
    }

  def typeExprType(typeExpr: Ast.TypeExpr): An[Types.Type] = typeExpr match {
    case Ast.TopType(_) => An.result(Types.Top)
    case Ast.BottomType(_) => An.result(Types.Bottom)
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.TupleType(ts, pos) => ???
    // case Ast.TupleType(ts, pos) => An.seq(ts.map(typeExprType)).flatMap { types =>
    //   Types.Tuple(types)
    // }
    case Ast.NamedType(nameWithPos, typeArgs, pos) =>
      namedType(nameWithPos, typeArgs, pos)
    case Ast.RecordType(fields, _) =>
      An.step(fields)(Namespace.empty[Types.Type]){ case (ns, Ast.FieldDecl(name, typeExpr, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        typeExprType(typeExpr).flatMap { typ =>
          An.fromSomeOrError(ns.add((name, typ)), duplicateName)
        }
      }.map(Types.Record(_))
    case Ast.FuncType(typeParams, parameter, result, _) => {
      TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withParamsEnv, typeParamCons) =>
        TypeExprAnalyzerImpl(withParamsEnv).funcType(typeParamCons, parameter, result)
      }
    }
  }

  def funcType(typeParamCons: Seq[TypeCons.ParamCon], parameter: Ast.TypeExpr, result: Ast.TypeExpr): An[Types.Type] =
    typeExprType(parameter).zip(typeExprType(result)).map { case (parameterTyp, resultTyp) =>
      Types.Func(typeParamCons, parameterTyp, resultTyp)
    }
}

object TypeExprAnalyzer {
  def namedNullaryType(env: Env, name: String, pos: Pos): An[Types.Type] =
    TypeExprAnalyzerImpl(env).namedType(Ast.NameWithPos(name, pos), Nil, pos)
  def typeExprType(env: Env, typeExpr: Ast.TypeExpr): An[Types.Type] =
    TypeExprAnalyzerImpl(env).typeExprType(typeExpr)
}
