package clara.analyzer.impl

import clara.asg.Types
import clara.ast.{SourceMessage, Pos}

import ai.x.safe._

object TypeAnalyzer {
  def instantiate(typeCon: Types.TypeCon, typeArgs: Seq[Types.Typ], pos: Pos) = typeCon match {
    // TODO remove duplication between alias and typedef
    case Types.AliasCon(name, params, typeTemplate) =>
      substitution(params, typeArgs, typeTemplate, pos).map { wrappedType =>
        Types.Alias(name, typeArgs, wrappedType)
      }
    case Types.UniqueCon(name, params, constructible, typeTemplate, uniq) =>
      substitution(params, typeArgs, typeTemplate, pos).map { wrappedType =>
        Types.Unique(name, typeArgs, constructible, wrappedType, uniq)
      }
    case Types.ParamCon(name, uniq) => An.result(Types.Param(name, uniq))
  }

  def expectAssignable(t1: Types.Typ, t2: Types.Typ, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }

  def substitution(params: Seq[Types.Param], args: Seq[Types.Typ], typeTemplate: Types.Typ, pos: Pos): An[Types.Typ] = {
    if (params.length === args.length) {
      An.result(Types.substituteParams(params.zip(args).toMap, typeTemplate))
    } else {
      // FIXME better error message: print out params and args
      An.error(SourceMessage(pos, safe"Expected ${params.length.toString} type arguments, but ${args.length.toString} given"))
    }
  }
}
