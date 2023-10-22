package clara.analyzer.impl

import clara.asg.{Types, Namespace}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

case class TypeExprAnalyzer(env: Env) {
  def walkTypeExpr(typeExpr: Ast.TypeExpr): An[Types.Type] = typeExpr match {
    case Ast.TopType(_) => An.result(Types.Top)
    case Ast.BottomType(_) => An.result(Types.Bottom)
    case Ast.UnitType(_) => An.result(Types.Uni)
    case Ast.TupleType(ts, pos) => ???
    case Ast.NamedType(Ast.NameWithPos(name, namePos), typeArgs, pos) =>
      env.useTypeCon(name, namePos).zip(An.seq(typeArgs.map(walkTypeExpr))).flatMap { case (typeCon, args) =>
        TypeInterpreter.instantiate(typeCon, args, pos)
      }
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
}

// object TypeExprAnalyzer {
//   def typ(env: Env)(typeExpr: Ast.TypeExpr): An[Types.Type] = {
//     def typ = typ(env)(_)

//     typeExpr match ...
//   }
// }
