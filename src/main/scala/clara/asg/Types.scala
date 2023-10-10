package clara.asg

import clara.ast.Pos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe._
import javax.swing.text.html.HTML.Tag

// FIXME move
class Uniq()
object Uniq {
  def apply() = new Uniq()
}

// FIXME move
object TypeCons {
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

  case class SolitaryTypeCon(
    typeDefKind: TypeDefKind.Solitary,
    name: String,
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon
}

// type lattice
object Types {
  sealed trait Type

  // primitive non-composite types
  // each object corresponds directly to the type§
  sealed trait Quark extends Type // FIXME useful?
  case object Top extends Quark
  case object Bottom extends Quark
  case object Uni extends Quark // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: Type, result: Type) extends Type
  case class Record(fields: Namespace[Type]) extends Type
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
  sealed trait Wrapper extends Nominal

  case class Alias(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Wrapper

  case class Tagged(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Wrapper

  case class Boxed(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Wrapper

  case class Opaque(
    con: TypeCons.SolitaryTypeCon,
  ) extends Nominal

  case class Singleton(
    con: TypeCons.SolitaryTypeCon,
  ) extends Nominal
  // ::tagged Box<A>: { value: A }
  // TaggedCon(Box, Seq(a), Record("value" -> a))

  // TODO ::alias Foo<A>
  // case class TypeLambda(typeParams: Seq[Param], typeTemplate: Type) extends Type

  def sameCon(t1: Nominal, t2: Nominal): Boolean = t1.con.uniq === t2.con.uniq
  // TODO unit test
  def isAssignable(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (_, Top)    => true
    case (Bottom, _) => true
    case (Uni, Uni)  => true
    case (Func(p1, r1), Func(p2, r2)) =>
      isAssignable(r1, r2) && isAssignable(p2, p1)
    case (r1: Record, r2: Record) =>
      r2.fields.entries.forall {
        case (name, t2) =>
          r1.fields.get(name).exists(t1 => isAssignable(t1, t2))
      }
      case (p1: Param, p2: Param) => sameCon(p1, p2)
      case (Alias(_, _, wrappedType), t2) => isAssignable(wrappedType, t2)
      case (t1, Alias(_, _, wrappedType)) => isAssignable(t1, wrappedType)
      // case Tagged(con, typeArgs, wrappedType) =>
      // case Boxed(con, typeArgs, wrappedType) =>
      case (o1: Opaque, o2: Opaque) => sameCon(o1, o2)
      // case Singleton(con) =>
    // case (t1: NominalApplied, t2: TaggedApplied) =>
    //   isAssignable(t1.wrappedType, t2.wrappedType)
    // case (t1: TaggedApplied, t2) => isAssignable(t1.wrappedType, t2)
    case _ => false
  }

  def substituteParams(substitutions: Map[Param, Type], typ: Type) = {
    def traverse(typ: Type): Type = typ match {
      case q: Quark => q
      case Func(parameter, result) =>
        Func(traverse(parameter), traverse(result))
      case Record(fields)           => Record(fields.mapValues(traverse))
      case p: Param => substitutions.getOrElse(p, p)
      // case Alias(con, typeArgs, wrappedType) =>
      // case Tagged(con, typeArgs, wrappedType) =>
      // case Boxed(con, typeArgs, wrappedType) =>
      // case Opaque(con) =>
      // case Singleton(con) =>
      // case Alias(name, wrappedType) => Alias(name, traverse(wrappedType))
      // case t: TaggedCon => t.copy(wrappedType = traverse(t.wrappedType))
      // case t: TaggedApplied => t.copy(typeArgs = t.typeArgs.map(traverse), wrappedType = traverse(t.wrappedType))
    }

    traverse(typ)
  }

  def toSource(typ: Type): String = typ match {
    case Top    => "*" // Anything
    case Bottom => "!" // Nothing
    case Uni    => "()"
    case Func(parameter, result) =>
      safe"${toSource(parameter)} => ${toSource(result)}"
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
    // FIXME copy-paste
    case Alias(con, typeArgs, _) => con.name + typeArgs.map(toSource).safeString("<", " ,", ">")
    case Tagged(con, typeArgs, _) => con.name + typeArgs.map(toSource).safeString("<", " ,", ">")
    case Boxed(con, typeArgs, _) => con.name + typeArgs.map(toSource).safeString("<", " ,", ">")
    case Opaque(con) => con.name
    case Singleton(con) => con.name
  }

}
