package clara.asg

import clara.util.Safe._

// TODO rename to Tag?
class Uniq()
object Uniq {
  def apply() = new Uniq()
}

// type lattice
object Types {
  sealed trait Type
  sealed trait MonoType extends Type // TODO: rename to TypeInst?

  // primitive non-composite types
  // each object corresponds directly to the type
  trait Quark extends MonoType
  case object Top extends Quark
  case object Bottom extends Quark
  case object Uni extends Quark // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: MonoType, result: MonoType) extends MonoType
  case class Record(fields: Namespace[MonoType]) extends MonoType

  // user defined nominal types
  // TODO: drop `name`s and lazily get name from Env?
  // TODO: add `definedAt: Pos` ?
  case class Alias(name: String, wrappedType: MonoType) extends MonoType
  // ::type (rename to ::tagged) is Unique wrapped inside Alias
  case class Unique(
      constructible: Boolean,
      wrappedType: MonoType,
      uniq: Uniq = new Uniq()
  ) extends MonoType

  // TODO: instead of FooCon(), Foo() have: Con<Foo>(Foo(), params) and Inst<Foo>(Foo(), args) ?
  case class Param(name: String, uniq: Uniq = new Uniq()) extends MonoType
  case class ForAll(typeParams: Seq[Param], typeTemplate: MonoType) extends Type
  case class Applied(typeArgs: Seq[MonoType], typeExpanded: MonoType)
      extends MonoType

  def maybeForAll(typeParams: Seq[Param], typeTemplate: MonoType) =
    typeParams match {
      case Nil => typeTemplate
      case _   => ForAll(typeParams, typeTemplate)
    }

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
    case (Alias(_, wrappedType), t2)     => isAssignable(wrappedType, t2)
    case (t1, Alias(_, wrappedType))     => isAssignable(t1, wrappedType)
    case (t1: Unique, t2: Unique)        => t1.uniq === t2.uniq
    case (Unique(_, wrappedType, _), t2) => isAssignable(wrappedType, t2)
    case (p1: Param, p2: Param)          => p1.uniq === p2.uniq
    case (t1: Applied, t2: Applied) =>
      isAssignable(t1.typeExpanded, t2.typeExpanded)
    case _ => false
  }

  def substituteParams(substitutions: Map[Param, MonoType], in: MonoType) = {
    def traverse(in: MonoType): MonoType = in match {
      case q: Quark => q
      case Func(parameter, result) =>
        Func(traverse(parameter), traverse(result))
      case Record(fields)           => Record(fields.mapValues(traverse))
      case Alias(name, wrappedType) => Alias(name, traverse(wrappedType))
      case Unique(constructible, wrappedType, uniq) =>
        Unique(constructible, traverse(wrappedType), uniq)
      case p: Param => substitutions.getOrElse(p, p)
      case Applied(typeArgs, typeExpanded) =>
        Applied(typeArgs.map(traverse), traverse(typeExpanded))
    }

    traverse(in)
  }

  def toSource(t: Type): String = t match {
    case Top    => "⊤" // Any // TODO looks too much like letter T?
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
    case Alias(name, _) => name
    case _: Unique =>
      throw new IllegalArgumentException("toSource(Unique) not defined") // FIXME
    case Param(name, _) => name
    case _: ForAll =>
      throw new IllegalArgumentException("toSource(ForAll) not defined") // FIXME
    case Applied(typeArgs, typeExpanded) =>
      toSource(typeExpanded) + typeArgs.map(toSource).safeString("<", " ,", ">")
  }

}
