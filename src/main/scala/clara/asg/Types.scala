package clara.asg

import clara.ast.Pos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe._

// FIXME move
class Uniq() {
  override def toString: String = this.getClass.getSimpleName() + this.hashCode()
}
object Uniq {
  def apply() = new Uniq()
}

// FIXME move
object TypeCons {
  import Impl._

  sealed trait TypeCon {
    def name: String
    def definedAt: Pos
    def uniq: Uniq
  }

  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class ParamCon(
    name: String,
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon

  case class WrapperTypeCon(
    typeDefKind: TypeDefKind.Wrapper,
    name: String,
    typeParams: Seq[ParamCon],
    wrappedType: Types.Type,
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon

  case class OpaqueTypeCon(
    name: String,
    typeParams: Seq[ParamCon],
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon

  case class SingletonTypeCon(
    name: String,
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon

  def toSource(con: TypeCon): String = con match {
    case con: ParamCon => con.name
    case con: WrapperTypeCon => toSourceWithTypeParams(con.name, con.typeParams)
    case con: OpaqueTypeCon => toSourceWithTypeParams(con.name, con.typeParams)
    case con: SingletonTypeCon => con.name
  }

  object Impl {
    import ToSourceImpl._

    def toSourceWithTypeParams(name: String, typeParams: Seq[ParamCon]): String =
      name + typeListSource(typeParams.map(toSource))
  }
}

// type lattice
object Types {
  import Impl._

  sealed trait Type

  // primitive non-composite types
  case object Top extends Type
  case object Bottom extends Type
  case object Uni extends Type // the unit type

  // primitive structural composite types
  case class Func(typeParams: Seq[TypeCons.ParamCon], parameter: Type, result: Type) extends Type
  object Func {
    def apply(parameter: Type, result: Type): Func = Func(Nil, parameter, result)
  }

  case class Record(fields: Namespace[Type]) extends Type
  object Record {
    def apply(fields: (String, Type)*): Record = Record(Namespace(fields:_*))
    def empty: Record = apply()
  }

  case class Tuple(types: Seq[Type]) extends Type

  case class Union(types: Seq[Type]) extends Type
  object Union {
    def apply(types: Seq[Type]): Union = new Union(types.flatMap {
      case nested: Union => nested.types
      case Bottom => Seq()
      case t => Seq(t)
    })
  }

  case class Intersection(types: Seq[Type]) extends Type
  object Intersection {
    def apply(types: Seq[Type]): Intersection = new Intersection(types.flatMap {
      case nested: Intersection => nested.types
      case Top => Seq()
      case t => Seq(t)
    })
  }

  // user defined nominal types
  sealed trait Nominal extends Type {
    def con: TypeCons.TypeCon
  }

  // type variable
  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class Param(con: TypeCons.ParamCon) extends Nominal

  // nominal composite types
  // TODO: drop `name`s and lazily get name from Env?
  // TODO: add `definedAt: Pos` ?
  // Unable to assign type
  //   ::tagged Foo (defined at firstfoo.clara:123)
  // to expected type
  //   ::tagged Foo (defined at anotherfoo.clara:123)

  case class Alias(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal

  case class Tagged(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal

  case class Boxed(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal

  // nominal non-composite types

  case class Opaque(
    con: TypeCons.OpaqueTypeCon,
    typeArgs: Seq[Type],
  ) extends Nominal

  case class Singleton(
    con: TypeCons.SingletonTypeCon,
  ) extends Nominal

  def sameCon(t1: Nominal, t2: Nominal): Boolean = t1.con.uniq === t2.con.uniq

  def isAssignable(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (_, Top)    => true
    case (Bottom, _) => true
    case (Uni, Uni)  => true
    case (Func(typeParams1, p1, r1), Func(typeParams2, p2, r2)) =>
      // FIXME handle type params: <A>A => A should be assignable to <B>B => B
      // substitute A for B in t2 an then check if r and p types are compatible
      isAssignable(p2, p1) && isAssignable(r1, r2)
    case (r1: Record, r2: Record) =>
      r2.fields.entries.forall { case (name, t2) =>
        r1.fields.get(name).exists(t1 => isAssignable(t1, t2))
      }
    case (Tuple(ts1), Tuple(ts2)) => ts1.length == ts2.length && ts1.zip(ts2).forall { case (t1, t2) =>
      isAssignable(t1, t2)
    }
    case (Union(ts1), t2) => ts1.forall(t1 => isAssignable(t1, t2))
    case (t1, Union(ts2)) => ts2.exists(t2 => isAssignable(t1, t2))
    case (Intersection(ts1), t2) => ts1.exists(t1 => isAssignable(t1, t2))
    case (t1, Intersection(ts2)) => ts2.forall(t2 => isAssignable(t1, t2))
    case (p1: Param, p2: Param) => sameCon(p1, p2)
    case (Alias(_, _, wrappedType), t2) => isAssignable(wrappedType, t2)
    case (t1, Alias(_, _, wrappedType)) => isAssignable(t1, wrappedType)
    case (t1: Tagged, t2: Tagged) => sameCon(t1, t2) && isAssignable(t1.wrappedType, t2.wrappedType)
    case (t1: Boxed, t2: Boxed) => sameCon(t1, t2) && isAssignable(t1.wrappedType, t2.wrappedType)
    case (t1: Opaque, t2: Opaque) => sameCon(t1, t2) &&
      t1.typeArgs.zip(t2.typeArgs).forall { case (t1a, t2a) =>
        isAssignable(t1a, t2a) && isAssignable(t2a, t1a)
      }
    case (t1: Singleton, t2: Singleton) => sameCon(t1, t2)
    case _ => false
  }

  // def leastCommonSupertype(t1: Type, t2: Type): Type = (t1, t2) match {
  //   case (_, Top)    => Top
  //   case (Top, _)    => Top
  //   case (t1, Bottom) => t1
  //   case (Bottom, t2) => t2
  //   case _  => Top // &
  // }

  def findSubstitutions(params: Set[Uniq], t1: Type, t2: Type): Map[Uniq, Type] = {
    def candidates(t1: Type, t2: Type): Seq[(Uniq, Type)] = (t1, t2) match {
      case (p: Param, t2) if params(p.con.uniq) => Seq(p.con.uniq -> t2)
      case (Func(Nil, p1, r1), Func(Nil, p2, r2)) => candidates(p1, p2) ++ candidates(r1, r2)
      case (r1: Record, r2: Record) =>
        r2.fields.entries.flatMap { case (name, t2) =>
          r1.fields.get(name).map(t1 => candidates(t1, t2)).getOrElse(Nil) // FIXME or should be error?
        }
      case (Tuple(ts1), Tuple(ts2)) => ts1.zip(ts2).flatMap { case (t1, t2) =>
        candidates(t1, t2)
      }
      case _ => Nil
    }

    val candidatesByParam = candidates(t1, t2).groupBy(_._1).map { case(_, candidateGroup) =>
      (candidateGroup.head._1, candidateGroup.map(_._2))
    }

    candidatesByParam.map { case(param, candidates) =>
      // FIXME
      // (param, leastCommonSupertype(candidates))
      (param, candidates.head)
    }
  }

  def substituteParams(substitutions: Map[Uniq, Type], typ: Type) = {
    def substitute(typ: Type): Type = typ match {
      case t @ (Top | Bottom | Uni) => t
      case Func(typeParams, parameter, result) =>
        // NOTE typeParams is not touched. If you are instantiating this func with type args, remember to Nil params
        Func(typeParams, substitute(parameter), substitute(result))
      case Record(fields) => Record(fields.mapValues(substitute))
      case Tuple(ts) => Tuple(ts.map(substitute))
      case Union(ts) => Union(ts.map(substitute))
      case Intersection(ts) => Intersection(ts.map(substitute))
      case p: Param => substitutions.getOrElse(p.con.uniq, p)
      case Alias(con, typeArgs, wrappedType) => Alias(con, typeArgs.map(substitute), substitute(wrappedType))
      case Tagged(con, typeArgs, wrappedType) => Tagged(con, typeArgs.map(substitute), substitute(wrappedType))
      case Boxed(con, typeArgs, wrappedType) => Boxed(con, typeArgs.map(substitute), substitute(wrappedType))
      case t: Opaque => t
      case t: Singleton => t
    }

    substitute(typ)
  }

  def toSource(typ: Type): String = typ match {
    case Top    => "*"
    case Bottom => "!"
    case Uni    => "()"
    case Func(typeParams, parameter, result) =>
      val paramsList = ToSourceImpl.typeListSource(typeParams.map(TypeCons.toSource))
      safe"$paramsList${toSource(parameter)} => ${toSource(result)}"
    case Record(fields) =>
      fields
        .mapValues(toSource)
        .entries
        .map {
          case (name, v) =>
            safe"$name: $v"
        }
        .safeString("{", ", ", "}")
    case Tuple(ts) => ts.map(toSource).safeString("(", ", ", ")")
    case Union(ts) => ts.map(toSource).safeString(" | ")
    case Intersection(ts) => ts.map(toSource).safeString(" & ")
    case Param(con) => con.name
    case Alias(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Tagged(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Boxed(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Opaque(con, typeArgs) => nominalToSource(con, typeArgs)
    case Singleton(con) => con.name
  }

  object Impl {
    import ToSourceImpl._

    def nominalToSource(con: TypeCons.TypeCon, typeArgs: Seq[Types.Type]): String =
      con.name + typeListSource(typeArgs.map(toSource))
  }
}

object ToSourceImpl {
  def typeListSource(items: Seq[String]): String =
    Some(items).filter(_.nonEmpty).map(_.safeString("<", ", ", ">")).getOrElse("")
}
