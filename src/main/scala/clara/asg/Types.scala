package clara.asg

import ai.x.safe._


class Uniq()
object Uniq {
  def apply() = new Uniq()
}

// type lattice
object Types {
  sealed trait Typ

  // primitive types
  // each object corresponds directly to the type
  case object Top extends Typ
  case object Bottom extends Typ
  case object Uni extends Typ // Unit

  // primitive structural composite types
  // every instance of case class corresponds to a type
  case class Func(parameter: Typ, result: Typ) extends Typ
  case class Record(fields: Namespace[Typ]) extends Typ

  // user defined nominal types
  // TODO: drop names and lazily get name from Env?
  // TODO: instead of FooCon(), Foo() have: Con<Foo>(Foo(), params) and Inst<Foo>(Foo(), args) ?
  sealed trait TypeCon
  case class ParamCon(name: String, uniq: Uniq = new Uniq()) extends TypeCon
  case class Param(name: String, uniq: Uniq) extends Typ
  // TODO remove duplication between alias and typedef
  case class AliasCon(name: String, params: Seq[Param], typeTemplate: Typ) extends TypeCon
  case class Alias(name: String, args: Seq[Typ], wrappedType: Typ) extends Typ
  case class UniqueCon(name: String, params: Seq[Param], constructible: Boolean, typeTemplate: Typ, uniq: Uniq = new Uniq()) extends TypeCon
  case class Unique(name: String, args: Seq[Typ], constructible: Boolean, wrappedType: Typ, uniq: Uniq) extends Typ

  // TODO unit test
  def isAssignable(t1: Typ, t2: Typ): Boolean =
    t2 === Top ||
    t1 === Bottom ||
    ((t1, t2) match {
      case (Func(p1, r1), Func(p2, r2)) => isAssignable(r1, r2) && isAssignable(p2, p1)
      case (Alias(_, _, wrappedType), t2) => isAssignable(wrappedType, t2)
      case (t1, Alias(_, _, wrappedType)) => isAssignable(t1, wrappedType)
      case (t1: Unique, t2: Unique) => t1.uniq === t2.uniq // optimisation
      case (Unique(_, _, _, wrappedType, _), t2) => isAssignable(wrappedType, t2)
      case _ => false
    }) ||
    t1 === t2 // deep equality

  def substituteParams(substitutions: Map[Param, Typ], in: Typ) = {
    def traverse(in: Typ): Typ = in match {
      // TODO maybe a trait for non-composite types that dont include other types
      case Top => Top
      case Bottom => Bottom
      case Uni => Uni
      case Func(parameter, result) => Func(traverse(parameter), traverse(result))
      case Record(fields) => Record(fields.mapValues(traverse))
      case Alias(name, params, wrappedType) => Alias(name, params, traverse(wrappedType))
      case Unique(name, args, constructible, wrappedType, uniq) => Unique(name, args.map(traverse), constructible, traverse(wrappedType), uniq)
      case p: Param => substitutions.getOrElse(p, p)
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
    case Param(name, _) => name
    // TODO remove duplication between alias and typedef
    case Alias(name, args, _) => safe"$name${typeArgsToSource(args)}"
    case Unique(name, args, _, _, _) => safe"$name${typeArgsToSource(args)}"
  }

  def typeArgsToSource(args: Seq[Typ]): String = args match {
    case Nil => ""
    case _ => args.map(toSource(_)).safeMkString("<", ", ", ">")
  }

}
