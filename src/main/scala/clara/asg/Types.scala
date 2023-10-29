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
  // each object corresponds directly to the type§
  case object Top extends Type
  case object Bottom extends Type
  case object Uni extends Type // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: Type, result: Type) extends Type
  case class PolyFunc(typeParams: Seq[TypeCons.ParamCon], parameter: Type, result: Type) extends Type

  case class Record(fields: Namespace[Type]) extends Type
  object Record {
    def apply(fields: (String, Type)*): Record = Record(Namespace(fields:_*))
    def empty: Record = apply()
  }

  // TODO case class Tuple(ts: Seq[Type]) extends Type

  sealed trait Nominal extends Type {
    def con: TypeCons.TypeCon
  }

  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class Param(con: TypeCons.ParamCon) extends Nominal

  // user defined nominal types
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
    case (Func(p1, r1), Func(p2, r2)) =>
      isAssignable(r1, r2) && isAssignable(p2, p1)
    case (r1: Record, r2: Record) =>
      r2.fields.entries.forall { case (name, t2) =>
        r1.fields.get(name).exists(t1 => isAssignable(t1, t2))
      }
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

  def substituteParams(substitutions: Map[Param, Type], typ: Type) = {
    def substitute(typ: Type): Type = typ match {
      case t @ (Top | Bottom | Uni) => t
      case Func(parameter, result) =>
        Func(substitute(parameter), substitute(result))
      case Record(fields)           => Record(fields.mapValues(substitute))
      case p: Param => substitutions.getOrElse(p, p)
      case Alias(con, typeArgs, wrappedType) => Alias(con, typeArgs.map(substitute), substitute(wrappedType))
      case Tagged(con, typeArgs, wrappedType) => Tagged(con, typeArgs.map(substitute), substitute(wrappedType))
      case Boxed(con, typeArgs, wrappedType) => Boxed(con, typeArgs.map(substitute), substitute(wrappedType))
      case t: Opaque => t
      case t: Singleton => t
    }

    substitute(typ)
  }

  def toSource(typ: Type): String = typ match {
    case Top    => "*" // Anything
    case Bottom => "!" // Nothing
    case Uni    => "()"
    case Func(parameter, result) =>
      safe"${toSource(parameter)} => ${toSource(result)}"
    case PolyFunc(typeParams, parameter, result) =>
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
