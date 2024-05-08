package clara.analyzer.impl

import clara.asg.{Uniq, Types}
import clara.asg.Types.{Func, Nominal, Alias, Tagged, Union, Boxed, Opaque, Singleton, Param, Type}
import clara.asg.TypeCons.{TypeCon, WrapperTypeCon, OpaqueTypeCon, SingletonTypeCon, ParamCon}
import clara.ast.{SourceMessage, Pos}
import clara.ast.Ast.TypeDefKind

import clara.util.Safe._
import clara.asg.TypeCons


object TypeInterpreter {
  import Impl._

  def instantiate(con: TypeCon, typeArgs: Seq[Type], pos: Pos): An[Nominal] = con match {
    case con @ WrapperTypeCon(typeDefKind, _, typeParams, wrappedType, _, _) => {
      matchTypeArgs(typeParams, typeArgs, con, pos).
        map(substitutions => Types.substituteParams(substitutions, wrappedType)).map { wrappedTypeSubstituted =>
          typeDefKind match {
            case TypeDefKind.Alias => Alias(con, typeArgs, wrappedTypeSubstituted)
            case TypeDefKind.Tagged => Tagged(con, typeArgs, wrappedTypeSubstituted)
            case TypeDefKind.Boxed => Boxed(con, typeArgs, wrappedTypeSubstituted)
          }
        }
    }
    case con @ OpaqueTypeCon(_, typeParams, _, _) =>
      matchTypeArgs(typeParams, typeArgs, con, pos).map(_ => Opaque(con, typeArgs))
    case con: SingletonTypeCon =>
      matchTypeArgs(Nil, typeArgs, con, pos).map(_ => Singleton(con))
    case con: ParamCon =>
      matchTypeArgs(Nil, typeArgs, con, pos).map(_ => Param(con))
  }

  def expectAssignable(t1: Type, t2: Type, pos: Pos): An[Unit] = Types.isAssignable(t1, t2) match {
    case true =>  An.result(())
    case false => An.error(SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`"))
  }

  def expandMethodTargets(topLevelTypeCon: TypeCon, pos: Pos): An[Set[TypeCon]] = {

    def methodTargets(con: TypeCon): An[Set[TypeCon]] = con match {
      case con @ WrapperTypeCon(TypeDefKind.Alias, _, _, wrappedType, _, _) =>
        aliasWrappedMethodTargets(wrappedType).map { wrappedTargets => Set(con) ++ wrappedTargets }
      case con => An.result(Set(con))
    }

    def aliasWrappedMethodTargets(wrappedType: Type): An[Set[TypeCons.TypeCon]] = wrappedType match {
      case typ: Nominal => methodTargets(typ.con)
      case Union(types) => An.seq(types.map {
        case t: Nominal => An.result(t.con)
        case t => An.error(SourceMessage(pos, safe"Cannot have methods for type `${TypeCons.toSource(topLevelTypeCon)}` because it expands into a union type with non-nominal member type `${Types.toSource(t)}`"))
      }).map(_.toSet)
      // FIXME exhaustive
    }

    methodTargets(topLevelTypeCon)
  }

  object Impl {
    def mismatchingTypeArgsMessage(paramCons: Seq[ParamCon], args: Seq[Type], con: TypeCon, pos: Pos) = {
      // FIXME better error message: print out params and args

      val message = if (paramCons.length === 0) {
        safe"Type `${TypeCons.toSource(con)}` does not take any type arguments, but ${args.length.toString} given"
      } else {
        safe"Type `${TypeCons.toSource(con)}` expects ${paramCons.length.toString} type arguments, but ${args.length.toString} given"
      }

      SourceMessage(pos, message)
    }

    def matchTypeArgs(paramCons: Seq[ParamCon], args: Seq[Type], con: TypeCon, pos: Pos): An[Map[Uniq, Type]] = {
      if (paramCons.length === args.length) {
        An.result(paramCons.map(_.uniq).zip(args).toMap)
      } else {
        An.error(mismatchingTypeArgsMessage(paramCons, args, con, pos))
      }
    }
  }
}
