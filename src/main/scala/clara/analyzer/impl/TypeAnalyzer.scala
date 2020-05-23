package clara.analyzer.impl

import clara.asg.Types
import clara.asg.Types.{Applied, ForAll, MonoType, Param, Typ}
import clara.ast.{SourceMessage, Pos}

import ai.x.safe._

object TypeAnalyzer {
  def instantiate(typ: Typ, typeArgs: Seq[MonoType], pos: Pos): An[MonoType] = typ match {
    case ForAll(typeParams, typeTemplate) =>
      applyTypeArgs(typeParams, typeArgs, typeTemplate, pos).map { typeExpanded =>
        Applied(typeArgs, typeExpanded)
      }
    case mt: MonoType => applyTypeArgs(Nil, typeArgs, mt, pos)
  }

  def expectAssignable(t1: Typ, t2: Typ, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }

  def applyTypeArgs(params: Seq[Param], args: Seq[MonoType], typeTemplate: MonoType, pos: Pos): An[MonoType] = {
    if (params.length === args.length) {
      if (params.length === 0) {
        An.result(typeTemplate)
      } else {
        An.result(Types.substituteParams(params.zip(args).toMap, typeTemplate))
      }
    } else {
      // FIXME better error message: print out params and args
      An.error(SourceMessage(pos, safe"Expected ${params.length.toString} type arguments, but ${args.length.toString} given"))
    }
  }
}
