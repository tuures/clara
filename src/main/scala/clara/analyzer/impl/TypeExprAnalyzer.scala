package clara.analyzer.impl

import clara.asg.{Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._
import clara.asg.TypeCons

case class TypeExprAnalyzerImpl(env: Env) {
  def namedType(nsPath: Seq[Ast.NameWithPos], name: Ast.NameWithPos, typeArgs: Seq[Ast.TypeExpr], pos: Pos): An[Types.Type] =
    NameResolver.typeCon(env, nsPath, name.name, name.pos).zip(An.seq(typeArgs.map(typeExprType))).flatMap {
      case (typeCon, args) => TypeInterpreter.instantiate(typeCon, args, pos)
    }

  def typeExprType(typeExpr: Ast.TypeExpr): An[Types.Type] = typeExpr match {
    case Ast.TopType(_) => An.valid(Types.Top)
    case Ast.BottomType(_) => An.valid(Types.Bottom)
    case Ast.UnitType(_) => An.valid(Types.Uni)
    case Ast.TupleType(ts, _) => An.seq(ts.map(typeExprType)).map { types =>
      Types.Tuple(types)
    }
    case Ast.NamedType(nsPath, name, typeArgs, pos) => namedType(nsPath, name, typeArgs, pos)
    case Ast.RecordType(fields, _) =>
      An.step(fields)(Namespace.empty[Types.Type]){ case (ns, Ast.FieldDecl(name, typeExpr, pos)) =>
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate field name `$name`")

        typeExprType(typeExpr).flatMap { typ =>
          An.fromSomeOrError(ns.add((name, typ)), duplicateName)
        }
      }.map(Types.Record(_))
    case Ast.UnionType(ts, _) => An.seq(ts.map(typeExprType)).map { types =>
      Types.Union(types)
    }
    case Ast.IntersectionType(ts, _) => An.seq(ts.map(typeExprType)).map { types =>
      Types.Intersection(types)
    }
    case Ast.FuncType(typeParams, parameter, result, _) => {
      TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (withTypeParamsEnv, typeParamCons) =>
        TypeExprAnalyzerImpl(withTypeParamsEnv).funcType(typeParamCons, parameter, result)
      }
    }
  }

  def funcType(typeParamCons: Seq[TypeCons.ParamCon], parameter: Ast.TypeExpr, result: Ast.TypeExpr): An[Types.Type] =
    typeExprType(parameter).zip(typeExprType(result)).map { case (parameterTyp, resultTyp) =>
      Types.Func(typeParamCons, parameterTyp, resultTyp)
    }
}

object TypeExprAnalyzer {
  def namedTypeCon(env: Env, n: Ast.NameWithPos): An[TypeCons.TypeCon] =
    NameResolver.typeCon(env, Nil, n.name, n.pos)
  def namedNullaryType(env: Env, name: String, pos: Pos): An[Types.Type] =
    TypeExprAnalyzerImpl(env).namedType(Nil, Ast.NameWithPos(name, pos), Nil, pos)
  def typeExprType(env: Env, typeExpr: Ast.TypeExpr): An[Types.Type] =
    TypeExprAnalyzerImpl(env).typeExprType(typeExpr)
}
