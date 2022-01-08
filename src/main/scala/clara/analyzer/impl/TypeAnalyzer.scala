package clara.analyzer.impl

import clara.asg.Types
import clara.asg.Types.{Param, Type, TaggedCon, TaggedApplied}
import clara.ast.{SourceMessage, Pos}

import clara.util.Safe._

object TypeAnalyzer {
  import Impl._

  def applyTypeArgs(typ: Type, typeArgs: Seq[Type], pos: Pos): An[Type] = typ match {
    case TaggedCon(name, typeParams, wrappedType, constructible, uniq) =>
      validateArgsAndSubstituteParams(typeParams, typeArgs, wrappedType, pos).map { wrappedTypeSubstituted =>
        TaggedApplied(name, typeArgs, wrappedTypeSubstituted, constructible, uniq)
      }
    case t: Type => doesNotTakeArgsError(t, typeArgs, pos)
  }

  def validateArgsAndSubstituteParams(params: Seq[Param], args: Seq[Type], typ: Type, pos: Pos): An[Type] = {
    if (params.length === args.length) {
      if (params.length === 0) {
        An.result(typ)
      } else {
        An.result(Types.substituteParams(params.zip(args).toMap, typ))
      }
    } else {
      if (params.length === 0) {
        doesNotTakeArgsError(typ, args, pos)
      }
      // FIXME better error message: print out params and args
      An.error(SourceMessage(pos, safe"Type `${Types.toSource(typ)}` expects ${params.length.toString} type arguments, but ${args.length.toString} given"))
    }
  }

  def expectAssignable(t1: Type, t2: Type, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }

  object Impl {
    def doesNotTakeArgsError(t: Type, args: Seq[Type], pos: Pos) = An.error(SourceMessage(pos, safe"Type `${Types.toSource(t)}` does not take any type arguments, but ${args.length.toString} given"))
  }
}
