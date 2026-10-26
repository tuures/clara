package clara.asg

import clara.ast.Pos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe._

// FIXME move
object TypeCons {
  import Types._
  import Impl._

  sealed trait TypeCon {
    def name: String
    def definedAt: Pos
    // FIXME: rename to typeConId
    def uniq: TypeConId
  }

  // TODO: allow narrowing Param with boundaries/kind, now it's always "extends Top"
  case class ParamCon(
    name: String,
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  case class WrapperTypeCon(
    typeDefKind: TypeDefKind.Wrapper,
    name: String,
    typeParams: Seq[ParamCon],
    definedInNs: Seq[String],
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  case class OpaqueTypeCon(
    name: String,
    typeParams: Seq[ParamCon],
    definedInNs: Seq[String],
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  case class SingletonTypeCon(
    name: String,
    definedInNs: Seq[String],
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  def pseudoInstantiate(con: TypeCon): Type = con match {
    case con: WrapperTypeCon => wrapper(con, con.typeParams.map(Param))
    case con @ OpaqueTypeCon(_, typeParams, _, _, _) => Opaque(con, typeParams.map(Param))
    case con: SingletonTypeCon => Singleton(con)
    case con: ParamCon => Param(con)
  }

  // a namespace's member a bare namespace `Bar` means; a tagged or boxed type's companion has its constructor as it
  val ConstructorName = "apply"

  def wrapper(con: WrapperTypeCon, typeArgs: Seq[Type]): Wrapper = con.typeDefKind match {
    case TypeDefKind.Alias => Alias(con, typeArgs)
    case TypeDefKind.Tagged => Tagged(con, typeArgs)
    case TypeDefKind.Boxed => Boxed(con, typeArgs)
  }

  // TODO: could be replaced with toSource(pseudoInstantiate(con)) on the callsite?
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
  import ToSourceImpl._

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
    def apply(types: Seq[Type]): Type = types.flatMap {
      case nested: Union => nested.types
      case Bottom => Seq()
      case t => Seq(t)
    }.distinct match {
      case Nil => Bottom
      case Seq(single) => single
      case ts => new Union(ts)
    }
  }

  case class Intersection(types: Seq[Type]) extends Type
  object Intersection {
    // only trivial simplification so that types are shown as written, see TypeInterpreterImpl.normalizeHead for the
    // checking form
    def apply(types: Seq[Type]): Type = types.flatMap {
      case nested: Intersection => nested.types
      case Top => Seq()
      case t => Seq(t)
    }.distinct match {
      case Nil => Top
      case Seq(single) => single
      case ts => new Intersection(ts)
    }
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
  // TODO add more verbose/useful error messages for assignability failure with breakdown etc
  // Unable to assign type
  //   ::tagged Foo (defined at firstfoo.clara:123)
  // to expected type
  //   ::tagged Foo (defined at anotherfoo.clara:123)

  // a type of a wrapper type con, what it wraps is computed when needed (see wrappedType)
  sealed trait Wrapper extends Nominal {
    def con: TypeCons.WrapperTypeCon
    def typeArgs: Seq[Type]
    // INTERNAL, only set by TypeInterpreterImpl.expandUnions and only for checking: the wrapped type narrowed to one
    // member of its unions, e.g. `Tag<Red | Blue>` (with `::tagged Tag<A>: A`) expands to a `Tag` wrapping `Red` and
    // one wrapping `Blue`, which aren't types of their own (their wrapped types don't follow from their type args).
    // Never keep or show such a type. Can go once same-con wrappers compare by their type args according to the
    // con's variance (see the TODO at TypeInterpreterImpl.isAssignableWrapper)
    def expandedWrappedType: Option[Type] = None
  }

  case class Alias(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
  ) extends Wrapper {
    require(con.typeDefKind == TypeDefKind.Alias, "Alias type constructor must be of kind Alias")
  }

  case class Tagged(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    override val expandedWrappedType: Option[Type] = None,
  ) extends Wrapper {
    require(con.typeDefKind == TypeDefKind.Tagged, "Tagged type constructor must be of kind Tagged")
  }

  case class Boxed(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    override val expandedWrappedType: Option[Type] = None,
  ) extends Wrapper {
    require(con.typeDefKind == TypeDefKind.Boxed, "Boxed type constructor must be of kind Boxed")
  }

  // nominal non-composite types

  case class Opaque(
    con: TypeCons.OpaqueTypeCon,
    typeArgs: Seq[Type],
  ) extends Nominal

  case class Singleton(
    con: TypeCons.SingletonTypeCon,
  ) extends Nominal

  def sameCon(t1: Nominal, t2: Nominal): Boolean = t1.con.uniq === t2.con.uniq

  // def leastCommonSupertype(t1: Type, t2: Type): Type = (t1, t2) match {
  //   case (_, Top)    => Top
  //   case (Top, _)    => Top
  //   case (t1, Bottom) => t1
  //   case (Bottom, t2) => t2
  //   case _  => Top // &
  // }

  def findSubstitutions(params: Set[TypeConId], parameterType: Type, argumentType: Type): Map[TypeConId, Type] = {
    def candidates(t1: Type, t2: Type): Seq[(TypeConId, Type)] = (t1, t2) match {
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

    candidates(parameterType, argumentType).groupMap(_._1)(_._2).map { case(param, candidates) =>
      // FIXME something like <A> (A, A) => A called with (Uni, Top) will find A -> Seq(Uni, Top) but the head
      // turns that into <A = Uni> (Uni, Uni) => Uni which is completely wrong.
      // Proper solution is to use a least common supertype for covariant occurrences and a greatest common subtype for contravariant occurrences
      (param, candidates.head)
    }
  }

  def substituteParams(substitutions: Map[TypeConId, Type], typ: Type) = {
    def substitute(typ: Type): Type = typ match {
      case t @ (Top | Bottom | Uni) => t
      case Func(typeParams, parameter, result) =>
        // NOTE no need to touch typeParams because they are always fresh with distinct uniqs for each func
        Func(typeParams, substitute(parameter), substitute(result))
      case Record(fields) => Record(fields.mapValues(substitute))
      case Tuple(ts) => Tuple(ts.map(substitute))
      case Union(ts) => Union(ts.map(substitute))
      case Intersection(ts) => Intersection(ts.map(substitute))
      case p: Param => substitutions.getOrElse(p.con.uniq, p)
      case Alias(con, typeArgs) => Alias(con, typeArgs.map(substitute))
      case Tagged(con, typeArgs, expandedWrappedType) => Tagged(con, typeArgs.map(substitute), expandedWrappedType.map(substitute))
      case Boxed(con, typeArgs, expandedWrappedType) => Boxed(con, typeArgs.map(substitute), expandedWrappedType.map(substitute))
      case Opaque(con, typeArgs) => Opaque(con, typeArgs.map(substitute))
      case t: Singleton => t
    }

    substitute(typ)
  }

  def toSource(typ: Type): String = typ match {
    case Top    => "*"
    case Bottom => "!"
    case Uni    => "()"
    case Func(typeParams, parameter, result) =>
      val paramsList = typeListSource(typeParams.map(TypeCons.toSource))
      safe"$paramsList${toSource(parameter)} => ${toSource(result)}"
    case Record(fields) =>
      fields.mapValues(toSource).entries.map { case (name, v) => safe"$name: $v" }.safeString("{", ", ", "}")
    case Tuple(ts) => ts.map(toSource).safeString("(", ", ", ")")
    case Union(ts) => ts.map(toSourceWithAutoParens).safeString(" | ")
    case Intersection(ts) => ts.map(toSourceWithAutoParens).safeString(" & ")
    case Param(con) => con.name
    case t: Wrapper => nominalToSource(t.con, t.typeArgs)
    case Opaque(con, typeArgs) => nominalToSource(con, typeArgs)
    case Singleton(con) => con.name
  }

}

object ToSourceImpl {
  import Types.{Type, Func, Union, Intersection, toSource}

  def typeListSource(items: Seq[String]): String =
    Some(items).filter(_.nonEmpty).map(_.safeString("<", ", ", ">")).getOrElse("")

  def nominalToSource(con: TypeCons.TypeCon, typeArgs: Seq[Type]): String =
    con.name + typeListSource(typeArgs.map(toSource))

  def parens(str: String) = s"($str)"

  def toSourceWithAutoParens(typ: Type): String = typ match {
    case _: Func | _: Union | _: Intersection => parens(toSource(typ))
    case _ => toSource(typ)
  }
}
