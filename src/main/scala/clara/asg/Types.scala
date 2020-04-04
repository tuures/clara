package clara.asg

import ai.x.safe._


// type lattice
object Types {
  sealed trait Typ
  // TODO figure out better but short names
  sealed trait Populated extends Typ // everything except Bottom
  sealed trait Empty extends Typ

  // fundamental primitive types
  case object Top extends Populated
  case object Bottom extends Empty
  case object Uni extends Populated // Unit

  // primitive composite types
  case class PopulatedAlias(name: String, wrappedType: Populated) extends Populated
  case class BottomAlias(name: String) extends Empty

  case class Func(parameter: Typ, result: Typ) extends Populated

  class Uniq()
  object Uniq {
    def apply() = new Uniq()
  }
  case class Unique(name: String, wrappedType: Populated, uniq: Uniq = new Uniq()) extends Populated

  case class Record(fields: Namespace[Typ]) extends Populated

  // TODO unit test
  def isAssignable(t1: Typ, t2: Typ): Boolean =
    t2 === Top ||
    t1 === Bottom ||
    ((t1, t2) match {
      case (PopulatedAlias(_, wrappedType), t2) => isAssignable(wrappedType, t2)
      case (t1, PopulatedAlias(_, wrappedType)) => isAssignable(t1, wrappedType)
      case (t1: BottomAlias, _) => true
      case (Func(p1, r1), Func(p2, r2)) => isAssignable(r1, r2) && isAssignable(p2, p1)
      case (t1: Unique, t2: Unique) => t1.uniq === t2.uniq // optimisation
      case (Unique(_, wrappedType, _), t2) => isAssignable(wrappedType, t2)
      case _ => false
    }) ||
    t1 === t2 // deep equality

  def toSource(t: Typ): String = t match {
    case Top => "⊤" // Any
    case Bottom => "⊥" // Nothing
    case Uni => "()"
    case Func(parameter, result) => safe"${toSource(parameter)} => ${toSource(result)}"
    case u: Unique => u.name
    case Record(fields) => fields.mapValues(toSource).entries.map { case (name, v) =>
      safe"$name: $v"
    }.safeMkString("{", ", ", "}")
  }
}
