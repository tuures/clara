package clara.analyzer.impl

import clara.asg.{Types, TypeCons}
import clara.asg.Types.{Param, Type}
import clara.asg.TypeCons.{TypeCon, WrapperTypeCon, OpaqueTypeCon, SingletonTypeCon, ParamCon}
import clara.ast.{SourceMessage, Pos}
import clara.ast.Ast.TypeDefKind

import clara.util.Safe._
import clara.asg.TypeCons.SingletonTypeCon

// rename TypeInterpreter?
object TypeInterpreter {
  import Impl._

  def instantiate(typeCon: TypeCon, typeArgs: Seq[Type], pos: Pos): An[Type] = typeCon match {
    case con @ WrapperTypeCon(typeDefKind, _, typeParams, wrappedType, _, _) => {
      matchTypeArgs(typeParams, typeArgs, con, pos).
        map(substitutions => Types.substituteParams(substitutions, wrappedType)).map { wrappedTypeSubstituted =>
          typeDefKind match {
            case TypeDefKind.Alias => Types.Alias(con, typeArgs, wrappedTypeSubstituted)
            case TypeDefKind.Tagged => Types.Tagged(con, typeArgs, wrappedTypeSubstituted)
            case TypeDefKind.Boxed => Types.Boxed(con, typeArgs, wrappedTypeSubstituted)
          }
        }
    }
    case con @ OpaqueTypeCon(_, typeParams, _, _) =>
      matchTypeArgs(typeParams, typeArgs, con, pos).map(_ => Types.Opaque(con, typeArgs))
    case con: SingletonTypeCon => An.result(Types.Singleton(con))
    case paramCon: ParamCon => An.result(instantiateParam(paramCon))
  }

  def expectAssignable(t1: Type, t2: Type, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }

  object Impl {
    def instantiateParam(paramCon: ParamCon): Types.Param = Types.Param(paramCon)

    def mismatchingTypeArgsMessage(params: Seq[Param], args: Seq[Type], con: TypeCon, pos: Pos) = {
      // FIXME better error message: print out params and args

      val message = if (params.length === 0) {
        safe"Type `${con.name}` does not take any type arguments, but ${args.length.toString} given"
      } else {
        safe"Type `${con.name}` expects ${params.length.toString} type arguments, but ${args.length.toString} given"
      }

      SourceMessage(pos, message)
    }

    def matchTypeArgs(paramCons: Seq[ParamCon], args: Seq[Type], con: TypeCon, pos: Pos): An[Map[Param, Type]] = {
      val params = paramCons.map(instantiateParam(_))
      if (params.length === args.length) {
        if (params.length === 0) {
          An.result(Map.empty)
        } else {
          An.result(params.zip(args).toMap)
        }
      } else {
        An.error(mismatchingTypeArgsMessage(params, args, con, pos))
      }
    }
  }
}
