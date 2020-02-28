package clara.asg

import ai.x.safe._

// type lattice
object Types {
  sealed trait Typ
  sealed trait StructuralTyp extends Typ

  // fundamental primitive types
  case object Top extends Typ
  case object Bottom extends Typ
  case object Uni extends StructuralTyp // Unit

  // primitive composite types
  case class Func(parameter: Typ, result: Typ) extends StructuralTyp

  class Uniq()
  object Uniq {
    def apply() = new Uniq()
  }
  case class Unique(structure: StructuralTyp, uniq: Uniq = new Uniq()) extends Typ

  def isAssignable(t1: Typ, t2: Typ): Boolean =
    t1 === t2 ||
    t2 === Top ||
    t1 === Bottom ||
    ((t1, t2) match {
      case (Func(p1, r1), Func(p2, r2)) => isAssignable(r1, r2) && isAssignable(p2, p1)
    })
}
