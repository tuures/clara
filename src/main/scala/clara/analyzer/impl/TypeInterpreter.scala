package clara.analyzer.impl

import clara.asg.{Types, TypeCons}
import clara.asg.Types.{Param, Type}
import clara.ast.{SourceMessage, Pos}
import clara.ast.Ast.TypeDefKind

import clara.util.Safe._

// rename TypeInterpreter?
object TypeInterpreter {
  import Impl._

  // def instantiate(typeCon: TypeCons.TypeCon, typeArgs: Seq[Type], pos: Pos): An[Type] = typeCon match {
  //   case TypeCons.TypeDefCon(typeDefKind, name, typeParams, wrappedType, definedAt, uniq) =>
  //     typeDefKind match {
  //       case TypeDefKind.Alias =>
  //         validateArgsAndSubstituteParams(typeParams, typeArgs, wrappedType.get, pos).map { wrappedTypeSubstituted =>
  //           TaggedApplied(name, typeArgs, wrappedTypeSubstituted, constructible, uniq)
  //         }
  //       case TypeDefKind.Tagged =>
  //       case TypeDefKind.Boxed =>
  //       case TypeDefKind.Opaque =>
  //       case TypeDefKind.Singleton =>
  //     }
  //   case TypeCons.ParamCon(name, definedAt, uniq) =>
  // }

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
