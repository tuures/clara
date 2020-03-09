package clara.asg

import clara.ast.Pos

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
  case class Unique(name: String, structure: StructuralTyp, uniq: Uniq = new Uniq()) extends Typ

  def isAssignable(t1: Typ, t2: Typ): Boolean =
    t1 === t2 ||
    t2 === Top ||
    t1 === Bottom ||
    ((t1, t2) match {
      case (Func(p1, r1), Func(p2, r2)) => isAssignable(r1, r2) && isAssignable(p2, p1)
      case (t1: Unique, t2: Unique) => t1.uniq === t2.uniq // optimisation
      case _ => false
    })

  def toSource(t: Typ): String = t match {
    // TODO: parser rule for Top/Bottom literals
    case Top => "⊤"
    case Bottom => "⊥"
    case Uni => "()"
    case Func(parameter, result) => safe"${toSource(parameter)} => ${toSource(result)}"
    case u: Unique => u.name
  }
}
