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
    wrappedType: Types.Type,
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  case class OpaqueTypeCon(
    name: String,
    typeParams: Seq[ParamCon],
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  case class SingletonTypeCon(
    name: String,
    definedAt: Pos,
    uniq: TypeConId = TypeConId(),
  ) extends TypeCon

  def pseudoInstantiate(con: TypeCon): Type = con match {
    case con @ WrapperTypeCon(typeDefKind, _, typeParams, wrappedType, _, _) => {
      val typeArgs = typeParams.map(Param)

      typeDefKind match {
        case TypeDefKind.Alias => Alias(con, typeArgs, wrappedType)
        case TypeDefKind.Tagged => Tagged(con, typeArgs, wrappedType)
        case TypeDefKind.Boxed => Boxed(con, typeArgs, wrappedType)
      }
    }
    case con @ OpaqueTypeCon(_, typeParams, _, _) => Opaque(con, typeParams.map(Param))
    case con: SingletonTypeCon => Singleton(con)
    case con: ParamCon => Param(con)
  }

  def wrapperConstructorFunc(con: WrapperTypeCon): Func = {
    // TODO: is it safe to re-use type con's type params as type params for the func?
    Func(con.typeParams, con.wrappedType, pseudoInstantiate(con))
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
  import ToSourceImpl._, NormalizeImpl._, ExpandUnionsImpl._, AssignableImpl._, DisjointImpl._

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
    // only trivial simplification so that types are shown as written, see normalizeHead for the checking form
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

  case class Alias(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal {
    require(con.typeDefKind == TypeDefKind.Alias, "Alias type constructor must be of kind Alias")
  }

  case class Tagged(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal {
    require(con.typeDefKind == TypeDefKind.Tagged, "Tagged type constructor must be of kind Tagged")
  }

  case class Boxed(
    con: TypeCons.WrapperTypeCon,
    typeArgs: Seq[Type],
    wrappedType: Types.Type,
  ) extends Nominal {
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

  // Normal form used by the checking logic. Intersections become unions of intersections
  // ((A | B) & C becomes (A & C) | (B & C)), aliases unwrap, records merge and disjoint members reduce to Bottom.
  // Only the outermost level is normalized; nested types get normalized when a comparison reaches them,
  // which keeps this finite for recursive types.
  def normalizeHead(typ: Type): Type = typ match {
    case Intersection(types) => normalizeIntersection(types)
    case t => t
  }

  def isAssignable(t1: Type, t2: Type): Boolean = (t1, t2) match {
    // Check for equality before any decomposition or subtyping based rules
    case _ if t1 == t2 => true
    // Alias needs to be unwrapped first
    case (Alias(_, _, wrappedType), t2) => isAssignable(wrappedType, t2)
    case (t1, Alias(_, _, wrappedType)) => isAssignable(t1, wrappedType)
    case (_, Top)    => true
    case (Bottom, _) => true
    case (t1 @ (_: Record | _: Tuple), _) if hasEmptyMember(t1) => true
    // Only a source intersection needs normalizing, a target intersection is fully handled by the forall rule below
    case (NormalizableIntersection(normalized), t2) => isAssignable(normalized, t2)
    case (Func(typeParams1, p1, r1), Func(typeParams2, p2, r2)) =>
      typeParams1.length == typeParams2.length && {
        val substitutions = typeParams2.zip(typeParams1).map { case (param2, param1) =>
          param2.uniq -> Param(param1)
        }.toMap
        val p2Renamed = substituteParams(substitutions, p2)
        val r2Renamed = substituteParams(substitutions, r2)

        isAssignable(p2Renamed, p1) && isAssignable(r1, r2Renamed)
      }
    case (r1: Record, r2: Record) =>
      r2.fields.entries.forall { case (name, t2) =>
        r1.fields.get(name).exists(t1 => isAssignable(t1, t2))
      }
    case (Tuple(ts1), Tuple(ts2)) => isAssignableTuple(ts1, ts2)
    // Apply the universal (forall) union/intersection rules before the existential rules (exists) below
    case (Union(ts1), t2) => ts1.forall(t1 => isAssignable(t1, t2))
    case (t1, Intersection(ts2)) => ts2.forall(t2 => isAssignable(t1, t2))
    // A source intersection or target union needs only one sufficient part to match. When both apply, try both:
    // a single part may match the whole union (e.g. an alias of a union) or the whole intersection may match one branch
    case (Intersection(ts1), Union(ts2)) =>
      ts1.exists(isAssignable(_, t2)) || ts2.exists(isAssignable(t1, _)) || isAssignableExpanded(t1, t2)
    case (t1, Union(ts2)) => ts2.exists(t2 => isAssignable(t1, t2)) || isAssignableExpanded(t1, t2)
    case (Intersection(ts1), f2: Func) => ts1.exists(isAssignable(_, f2)) || isAssignableFuncs(ts1, f2)
    case (Intersection(ts1), t2) => ts1.exists(t1 => isAssignable(t1, t2))
    case (p1: Param, p2: Param) => sameCon(p1, p2)
    case (t1: Tagged, t2: Tagged) => sameCon(t1, t2) &&
      isAssignableWrapper(t1.con, t1.typeArgs, t2.typeArgs, t1.wrappedType, t2.wrappedType)
    case (t1: Tagged, t2) => isAssignable(t1.wrappedType, t2)
    case (t1: Boxed, t2: Boxed) => sameCon(t1, t2) &&
      isAssignableWrapper(t1.con, t1.typeArgs, t2.typeArgs, t1.wrappedType, t2.wrappedType)
    case (t1: Opaque, t2: Opaque) => sameCon(t1, t2) && isAssignableInvariantArgs(t1.typeArgs, t2.typeArgs)
    case (t1: Singleton, t2: Singleton) => sameCon(t1, t2)
    case _ => false
  }

  // True when no value can have both types. Conservative: false whenever overlap can't be ruled out.
  // Distinct opaque (host provided) types are assumed disjoint from each other and from Clara's own singleton and
  // boxed values. Host types that genuinely overlap must not be declared as separate opaque types,
  // e.g. ::alias Number = Int | Float instead of ::opaque Number
  def disjoint(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (Bottom, _) | (_, Bottom) => true
    case (Alias(_, _, wrappedType), t2) => disjoint(wrappedType, t2)
    case (t1, Alias(_, _, wrappedType)) => disjoint(t1, wrappedType)
    case (Union(ts), t2) => ts.forall(disjoint(_, t2))
    case (t1, Union(ts)) => ts.forall(disjoint(t1, _))
    case (Intersection(ts), t2) => ts.exists(disjoint(_, t2))
    case (t1, Intersection(ts)) => ts.exists(disjoint(t1, _))
    case (t1: Tagged, t2) => disjoint(t1.wrappedType, t2)
    case (t1, t2: Tagged) => disjoint(t1, t2.wrappedType)
    case (Tuple(ts1), Tuple(ts2)) =>
      ts1.length != ts2.length || ts1.zip(ts2).exists { case (t1, t2) => disjoint(t1, t2) }
    case (_: Tuple, t2) => isDisjointFromTuples(t2)
    case (t1, _: Tuple) => isDisjointFromTuples(t1)
    // only shared fields matter, e.g. {a: Red} and {b: Blue} share the value {a: Red, b: Blue}
    case (Record(fields1), Record(fields2)) =>
      fields1.entries.exists { case (name, t1) => fields2.get(name).exists(disjoint(t1, _)) }
    case (t1: Boxed, t2: Boxed) => !sameCon(t1, t2) || disjoint(t1.wrappedType, t2.wrappedType)
    case (t1: Singleton, t2: Singleton) => !sameCon(t1, t2)
    case (_: Singleton, _: Boxed) | (_: Boxed, _: Singleton) => true
    case (t1: Opaque, t2: Opaque) => !sameCon(t1, t2)
    case (_: Opaque, _: Singleton | _: Boxed) | (_: Singleton | _: Boxed, _: Opaque) => true
    case _ => false
  }

  def narrow(t1: Type, t2: Type): Type =
    if (isAssignable(t1, t2)) t1
    else if (isAssignable(t2, t1)) t2
    else Intersection(Seq(t1, t2))

  // Normalize type into tuple form. e.g. (Int, Bool) | (String, Bool) | Int gives (Int | String, Bool).
  // Types that can't be decomposed but could be such a tuple (e.g. Top or a type param) give Top.
  def tupleElementTypes(typ: Type, length: Int): Seq[Type] = typ match {
    case Tuple(ts) if ts.length == length => ts
    case Alias(_, _, wrappedType) => tupleElementTypes(wrappedType, length)
    case t: Tagged => tupleElementTypes(t.wrappedType, length)
    case Union(ts) => ts.map(tupleElementTypes(_, length)).transpose.map(Union(_))
    case t: Intersection => normalizeHead(t) match {
      case Intersection(members) => members.map(tupleElementTypes(_, length)).transpose.map(Intersection(_))
      case normalized => tupleElementTypes(normalized, length)
    }
    case t if disjoint(t, Tuple(Seq.fill(length)(Top))) => Seq.fill(length)(Bottom)
    case _ => Seq.fill(length)(Top)
  }

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
      case Alias(con, typeArgs, wrappedType) => Alias(con, typeArgs.map(substitute), substitute(wrappedType))
      case Tagged(con, typeArgs, wrappedType) => Tagged(con, typeArgs.map(substitute), substitute(wrappedType))
      case Boxed(con, typeArgs, wrappedType) => Boxed(con, typeArgs.map(substitute), substitute(wrappedType))
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
    case Alias(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Tagged(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Boxed(con, typeArgs, _) => nominalToSource(con, typeArgs)
    case Opaque(con, typeArgs) => nominalToSource(con, typeArgs)
    case Singleton(con) => con.name
  }

  object NormalizeImpl {
    def normalizeIntersection(types: Seq[Type]): Type =
      types.flatMap(flattenIntersectionMembers).distinct.span(!_.isInstanceOf[Union]) match {
        case (before, Union(alternatives) +: after) =>
          // map instead of flatMap because Union constructor flattens nested unions which
          // the recursive call to normalizeIntersection might produce
          Union(alternatives.map(t => normalizeIntersection(before ++ (t +: after))))
        case (members, _) => normalizeIntersectionMembers(members)
      }

    def flattenIntersectionMembers(typ: Type): Seq[Type] = typ match {
      case Intersection(types) => types.flatMap(flattenIntersectionMembers)
      case Alias(_, _, wrappedType) => flattenIntersectionMembers(wrappedType)
      case Top => Seq()
      case t => Seq(t)
    }

    def normalizeIntersectionMembers(types: Seq[Type]): Type = {
      val (records, others) = types.partitionMap {
        case record: Record => Left(record)
        case t => Right(t)
      }
      val members = if (records.isEmpty) others else others :+ mergeRecords(records)

      if (members.contains(Bottom) || members.combinations(2).exists { case Seq(t1, t2) => disjoint(t1, t2) }) Bottom
      else members match {
        case Seq() => Top
        case Seq(single) => single
        case many => new Intersection(many)
      }
    }

    // {a: X} & {a: Y, b: Z} = {a: X & Y, b: Z}
    def mergeRecords(records: Seq[Record]): Type = {
      val fields = records.flatMap(_.fields.entries).foldLeft(Namespace.empty[Type]) { case (fields, (name, t)) =>
        fields.addOrModify(name -> normalizeIntersection(fields.get(name).toSeq :+ t))
      }
      if (fields.entries.exists { case (_, t) => t == Bottom }) Bottom else Record(fields)
    }
  }

  object ExpandUnionsImpl {
    // Limits the combinations of expandUnions like TypeScript does, beyond it the type is left unexpanded
    val MaxUnionCombinations = 25

    // All combinations of the unions nested in a type: directly, through aliases, tuple elements, record fields,
    // intersection members and wrapped types of tagged and boxed types, e.g. (X | Y, Z) expands to (X, Z) and (Y, Z).
    // returns None if there are more than MaxUnionCombinations combinations.
    // NOTE the results are only for checking assignability, never keep or show them: expanded tagged and boxed types
    // have type args that don't match their wrapped type (see below) so toSource would show them wrong.
    def expandUnions(typ: Type): Option[Seq[Type]] = {
      def expandEach(types: Seq[Type]): Option[Seq[Seq[Type]]] =
        types.foldLeft(Option(Seq(Seq.empty[Type]))) { case (combinations, t) =>
          for {
            heads <- combinations
            alternatives <- expandUnions(t)
            expanded = for { head <- heads; alternative <- alternatives } yield head :+ alternative
            if expanded.length <= MaxUnionCombinations
          } yield expanded
        }

      typ match {
        case Union(ts) => ts.foldLeft(Option(Seq.empty[Type])) { case (alternatives, t) =>
          for {
            previous <- alternatives
            expanded <- expandUnions(t)
            if previous.length + expanded.length <= MaxUnionCombinations
          } yield previous ++ expanded
        }
        case Alias(_, _, wrappedType) => expandUnions(wrappedType)
        case Tuple(ts) => expandEach(ts).map(_.map(Tuple(_)))
        case Record(fields) =>
          // From N fields, expand each field's type if it contains unions
          // into flat list of N-tuples of all combinations of the field types
          // Thus those N-tuples can be zipped back with the field names
          val (names, ts) = fields.entries.unzip
          expandEach(ts).map(_.map(expanded => Record(names.zip(expanded): _*)))
        // Narrow only the wrapped type and keep the con and type args, e.g. Some<Red | Blue> expands to a Some wrapping
        // Red and a Some wrapping Blue. Same-con types are compared by their wrapped types, so these compare correctly.
        // Type args can't be narrowed in general since the union might not come from a type param,
        // e.g. ::tagged Counted<A> {value: A, size: Small | Large} becomes (internally for this logic):
        // Counted<A>{value: A, size: Small} | Counted<A>{value: A, size: Large}
        case t: Tagged => expandUnions(t.wrappedType).map {
          // did not contain an union to expand so keep the original tagged type
          case Seq(_) => Seq(t)
          case unionMembers => unionMembers.map(unionMember => t.copy(wrappedType = unionMember))
        }
        case t: Boxed => expandUnions(t.wrappedType).map {
          case Seq(_) => Seq(t)
          case unionMembers => unionMembers.map(unionMember => t.copy(wrappedType = unionMember))
        }
        case t: Intersection => normalizeHead(t) match {
          case Intersection(members) => expandEach(members).map(_.map(Intersection(_)))
          case normalized => expandUnions(normalized)
        }
        case t => Some(Seq(t))
      }
    }
  }

  object AssignableImpl {
    // Optimization: same as `case (t1: Intersection, t2) if normalizeHead(t1) != t1 => isAssignable(normalizeHead(t1), t2)`
    // but normalizes only once. normalizeHead is relatively heavy (distribution and pairwise disjointness checks), and an
    // extractor is needed to fall through to the later rules when the intersection is already normalized.
    object NormalizableIntersection {
      def unapply(typ: Intersection): Option[Type] = Some(normalizeHead(typ)).filter(_ != typ)
    }

    // Each combination of the nested unions of t1 must be assignable on its own, e.g. (X | Y, Z) <: (X, Z) | (Y, Z).
    // If the source itself is among the combinations nothing was split, so there's nothing (more) to expand.
    def isAssignableExpanded(t1: Type, t2: Type): Boolean = expandUnions(t1) match {
      case Some(combinations) if !combinations.contains(t1) => combinations.forall(isAssignable(_, t2))
      case _ => false
    }

    // An intersection of functions (e.g. the cases of #()) is assignable to a function when each combination of the
    // function's parameter is accepted by one of them, e.g. (True => S) & (False => S) <: Boolean => S
    def isAssignableFuncs(types: Seq[Type], f2: Func): Boolean = f2.typeParams.isEmpty && {
      val funcs = types.collect { case f: Func if f.typeParams.isEmpty => f }
      funcs.nonEmpty && expandUnions(f2.parameter).exists(_.forall { parameter =>
        funcs.exists(f => isAssignable(parameter, f.parameter) && isAssignable(f.result, f2.result))
      })
    }

    // A record or tuple with an empty member has no values, e.g. {a: Red & Blue}, so it's assignable to anything
    def hasEmptyMember(typ: Type): Boolean = typ match {
      case Record(fields) => fields.entries.exists { case (_, t) => isEmpty(t) }
      case Tuple(ts) => ts.exists(isEmpty)
      case _ => false
    }

    def isEmpty(typ: Type): Boolean = typ match {
      case Bottom => true
      case t: Intersection => normalizeHead(t) == Bottom
      case t => hasEmptyMember(t)
    }

    def isAssignableTuple(types1: Seq[Type], types2: Seq[Type]): Boolean =
      types1.length == types2.length && types1.zip(types2).forall { case (type1, type2) => isAssignable(type1, type2) }

    def isAssignableInvariantArgs(types1: Seq[Type], types2: Seq[Type]): Boolean =
      isAssignableTuple(types1, types2) && isAssignableTuple(types2, types1)

    // Variance of type params used in the wrapped type follows from comparing the wrapped types, e.g.
    // ::tagged Handler<A> (A => ()) is contravariant in A. Phantom (unused) type params are invariant.
    // TODO: with variance annotations, infer each type con's variance once from its definition (covariant: only in
    // output positions, contravariant: only in input positions i.e. flipped by func params, invariant: both,
    // phantom: unused) and compare same-con types by their type args according to it instead of by wrapped types.
    // Replaces isAssignableWrapper and mentionsParam, gives annotations something to be checked against
    // ("declared covariant but used as input"), makes unannotated phantom type params an error, and is cheaper and
    // terminates for recursive types since args are compared instead of expanded wrapped types.
    def isAssignableWrapper(
      con: TypeCons.WrapperTypeCon,
      typeArgs1: Seq[Type],
      typeArgs2: Seq[Type],
      wrappedType1: Type,
      wrappedType2: Type,
    ): Boolean = {
      val phantom = con.typeParams.map(param => !mentionsParam(param.uniq, con.wrappedType))
      def phantomArgs(typeArgs: Seq[Type]) = typeArgs.zip(phantom).collect { case (t, true) => t }

      isAssignableInvariantArgs(phantomArgs(typeArgs1), phantomArgs(typeArgs2)) && isAssignable(wrappedType1, wrappedType2)
    }

    def mentionsParam(uniq: TypeConId, typ: Type): Boolean = {
      def mentions(t: Type) = mentionsParam(uniq, t)
      typ match {
        case p: Param => p.con.uniq === uniq
        case Func(_, parameter, result) => mentions(parameter) || mentions(result)
        case Record(fields) => fields.entries.exists { case (_, t) => mentions(t) }
        case Tuple(ts) => ts.exists(mentions)
        case Union(ts) => ts.exists(mentions)
        case Intersection(ts) => ts.exists(mentions)
        case Alias(_, typeArgs, _) => typeArgs.exists(mentions)
        case Tagged(_, typeArgs, _) => typeArgs.exists(mentions)
        case Boxed(_, typeArgs, _) => typeArgs.exists(mentions)
        case Opaque(_, typeArgs) => typeArgs.exists(mentions)
        case Top | Bottom | Uni | _: Singleton => false
      }
    }
  }

  object DisjointImpl {
    def isDisjointFromTuples(typ: Type): Boolean = typ match {
      case Bottom | Uni | _: Record | _: Func | _: Singleton | _: Boxed | _: Opaque => true
      // can hold tuples (Top, Param), is one (Tuple), or is decomposed by disjoint before this is reached
      case Top | _: Tuple | _: Param | _: Union | _: Intersection | _: Alias | _: Tagged => false
    }
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
