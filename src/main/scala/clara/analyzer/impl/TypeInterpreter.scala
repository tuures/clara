package clara.analyzer.impl

import clara.asg.{Types, TypeCons}
import clara.asg.Types.{Param, Type}
import clara.asg.TypeCons.{TypeCon, WrapperTypeCon, SolitaryTypeCon, ParamCon}
import clara.ast.{SourceMessage, Pos}
import clara.ast.Ast.TypeDefKind

import clara.util.Safe._

// rename TypeInterpreter?
object TypeInterpreter {
  import Impl._

  def instantiate(typeCon: TypeCon, typeArgs: Seq[Type], pos: Pos): An[Type] = typeCon match {
    case con @ WrapperTypeCon(typeDefKind, _, typeParams, wrappedType, _, _) => typeDefKind match {
      case TypeDefKind.Alias =>
        validateArgsAndSubstituteParams(typeParams, typeArgs, wrappedType, pos).map { wrappedTypeSubstituted =>
          Types.Alias(con, typeArgs, wrappedTypeSubstituted)
        }
      case TypeDefKind.Tagged => ???
      case TypeDefKind.Boxed => ???
    }
    case con @ SolitaryTypeCon(typeDefKind, _, _, _) => typeDefKind match {
      case TypeDefKind.Opaque => An.result(Types.Opaque(con))
      case TypeDefKind.Singleton => An.result(Types.Singleton(con))
    }
    case paramCon: ParamCon => An.result(instantiateParam(paramCon))
  }

  def instantiateParam(paramCon: ParamCon): Types.Param = Types.Param(paramCon)

  def validateArgsAndSubstituteParams(paramCons: Seq[ParamCon], args: Seq[Type], typ: Type, pos: Pos): An[Type] = {
    val params = paramCons.map(instantiateParam(_))
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
