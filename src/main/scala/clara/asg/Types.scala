package clara.asg

import clara.ast.Pos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe._

// FIXME move
class Uniq()
object Uniq {
  def apply() = new Uniq()
}

// FIXME move
object TypeCons {
  sealed trait TypeCon

  case class TypeDefCon(
    typeDefKind: TypeDefKind,
    name: String,
    typeParams: Seq[ParamCon],
    wrappedType: Option[Types.Type],
    definedAt: Pos,
    uniq: Uniq = Uniq(),
  ) extends TypeCon

  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class ParamCon(
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
  trait Quark extends Type
  case object Top extends Quark
  case object Bottom extends Quark
  case object Uni extends Quark // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: Type, result: Type) extends Type
  case class Record(fields: Namespace[Type]) extends Type
  // TODO case class Tuple(ts: Seq[Type]) extends Type

  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class Param(con: TypeCons.ParamCon) extends Type

  // user defined nominal types
  // TODO: drop `name`s and lazily get name from Env?
  // TODO: add `definedAt: Pos` ?
  // Unable to assign type
  //   ::tagged Foo (defined at firstfoo.clara:123)
  // to expected type
  //   ::tagged Foo (defined at anotherfoo.clara:123)
  case class Alias(
    con: TypeCons.TypeDefCon,
    typeArgs: Seq[Type],
  ) extends Type

  // ::tagged Box<A>: { value: A }
  // TaggedCon(Box, Seq(a), Record("value" -> a))

  // TODO ::alias Foo<A>
  // case class TypeLambda(typeParams: Seq[Param], typeTemplate: Type) extends Type

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
      case (p1: Param, p2: Param) => p1.con.uniq === p2.con.uniq
      // case (Alias(_, wrappedType), t2)     => isAssignable(wrappedType, t2)
      // case (t1, Alias(_, wrappedType))     => isAssignable(t1, wrappedType)
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
      // case Alias(name, wrappedType) => Alias(name, traverse(wrappedType))
      case p: Param => substitutions.getOrElse(p, p)
      // case t: TaggedCon => t.copy(wrappedType = traverse(t.wrappedType))
      // case t: TaggedApplied => t.copy(typeArgs = t.typeArgs.map(traverse), wrappedType = traverse(t.wrappedType))
    }

    traverse(typ)
  }

  def toSource(typ: Type): String = typ match {
    case Top    => "⊤" // Anything // TODO looks too much like letter T?
    case Bottom => "⊥" // Nothing
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
    // case Alias(name, _) => name
    // case Param(name, _) => name
    // case t: TaggedCon => t.name + t.typeParams.map(toSource).safeString("<", " ,", ">")
    // case t: TaggedApplied => t.name + t.typeArgs.map(toSource).safeString("<", " ,", ">")
  }

}
