package clara.asg

import ai.x.safe._


class Uniq()
object Uniq {
  def apply() = new Uniq()
}

// type lattice
object Types {
  sealed trait Typ
  sealed trait MonoType extends Typ

  // primitive types
  // each object corresponds directly to the type
  case object Top extends MonoType
  case object Bottom extends MonoType
  case object Uni extends MonoType // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: MonoType, result: MonoType) extends MonoType
  case class Record(fields: Namespace[MonoType]) extends MonoType

  // user defined nominal types
  // TODO: drop names and lazily get name from Env?
  case class Alias(name: String, wrappedType: MonoType) extends MonoType
  case class Unique(constructible: Boolean, wrappedType: MonoType, uniq: Uniq = new Uniq()) extends MonoType

    // TODO: instead of FooCon(), Foo() have: Con<Foo>(Foo(), params) and Inst<Foo>(Foo(), args) ?
  case class Param(name: String, uniq: Uniq = new Uniq()) extends MonoType
  case class ForAll(typeParams: Seq[Param], typeTemplate: MonoType) extends Typ
  case class Applied(typeArgs: Seq[MonoType], typeExpanded: MonoType) extends MonoType

  def maybeForAll(typeParams: Seq[Param], typeTemplate: MonoType) = typeParams match {
    case Nil => typeTemplate
    case _ => ForAll(typeParams, typeTemplate)
  }

  // TODO unit test
  def isAssignable(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    case (_, Top) => true
    case (Bottom, _) => true
    case (Uni, Uni) => true
    case (Func(p1, r1), Func(p2, r2)) => isAssignable(r1, r2) && isAssignable(p2, p1)
    case (r1: Record, r2: Record) => r2.fields.entries.forall { case (name, t2) =>
      r1.fields.get(name).exists(t1 => isAssignable(t1, t2))
    }
    case (Alias(_, wrappedType), t2) => isAssignable(wrappedType, t2)
    case (t1, Alias(_, wrappedType)) => isAssignable(t1, wrappedType)
    case (t1: Unique, t2: Unique) => t1.uniq === t2.uniq
    case (Unique(_, wrappedType, _), t2) => isAssignable(wrappedType, t2)
    case (p1: Param, p2: Param) => p1.uniq === p2.uniq
    case (t1: Applied, t2: Applied) => isAssignable(t1.typeExpanded, t2.typeExpanded)
    case _ => false
  }

  def substituteParams(substitutions: Map[Param, MonoType], in: MonoType) = {
    def traverse(in: MonoType): MonoType = in match {
      // TODO maybe a trait for non-composite types that dont include other types
      case Top => Top
      case Bottom => Bottom
      case Uni => Uni
      case Func(parameter, result) => Func(traverse(parameter), traverse(result))
      case Record(fields) => Record(fields.mapValues(traverse))
      case Alias(name, wrappedType) => Alias(name, traverse(wrappedType))
      case Unique(constructible, wrappedType, uniq) => Unique(constructible, traverse(wrappedType), uniq)
      case p: Param => substitutions.getOrElse(p, p)
      case Applied(typeArgs, typeExpanded) => Applied(typeArgs.map(traverse), traverse(typeExpanded))
    }

    traverse(in)
  }

  def toSource(t: Typ): String = t match {
    case Top => "⊤" // Any
    case Bottom => "⊥" // Nothing
    case Uni => "()"
    case Func(parameter, result) => safe"${toSource(parameter)} => ${toSource(result)}"
    case Record(fields) =>
      fields.mapValues(toSource).entries.map { case (name, v) =>
        safe"$name: $v"
      }.safeMkString("{", ", ", "}")
    case Alias(name, _) => name
    case _: Unique => throw new IllegalArgumentException("toSource(Unique) not defined") // FIXME
    case Param(name, _) => name
    case _: ForAll => throw new IllegalArgumentException("toSource(ForAll) not defined") // FIXME
    case Applied(typeArgs, typeExpanded) => toSource(typeExpanded) + typeArgs.map(toSource).safeMkString("<", " ,", ">")
  }

}
