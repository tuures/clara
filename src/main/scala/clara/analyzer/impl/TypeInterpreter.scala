package clara.analyzer.impl

import clara.asg.{Namespace, Terms, TermLiteral, TypeConId, Types}
import clara.asg.Types._
import clara.asg.TypeCons.{TypeCon, WrapperTypeCon, OpaqueTypeCon, SingletonTypeCon, ParamCon}
import clara.ast.{Ast, SourceMessage, Pos}
import clara.ast.Ast.TypeDefKind

import clara.util.Safe._
import clara.asg.TypeCons

object TypeInterpreter {
  import TypeInterpreterImpl._

  def instantiate(con: TypeCon, typeArgs: Seq[Type], pos: Pos): An[Nominal] = con match {
    case con: WrapperTypeCon => matchTypeArgs(con.typeParams, typeArgs, con, pos).map(_ => TypeCons.wrapper(con, typeArgs))
    case con @ OpaqueTypeCon(_, typeParams, _, _, _) =>
      matchTypeArgs(typeParams, typeArgs, con, pos).map(_ => Opaque(con, typeArgs))
    case con: SingletonTypeCon =>
      matchTypeArgs(Nil, typeArgs, con, pos).map(_ => Singleton(con))
    case con: ParamCon =>
      matchTypeArgs(Nil, typeArgs, con, pos).map(_ => Param(con))
  }

  def expectAssignable(t1: Type, t2: Type, pos: Pos): An[Unit] = withTypeStructures { impl =>
    An.errorIf(!impl.isAssignable(t1, t2))(
      SourceMessage(pos, safe"Type `${Types.toSource(t1)}` is not assignable to type `${Types.toSource(t2)}`")
    )
  }

  def disjoint(t1: Type, t2: Type): An[Boolean] = withTypeStructures(impl => An.valid(impl.disjoint(t1, t2)))
  def narrow(t1: Type, t2: Type): An[Type] = withTypeStructures(impl => An.valid(impl.narrow(t1, t2)))
  def tupleElementTypes(typ: Type, length: Int): An[Seq[Type]] = withTypeStructures(impl => An.valid(impl.tupleElementTypes(typ, length)))
  def wrappedType(t: Wrapper): An[Type] = withTypeStructures(impl => An.valid(impl.wrappedType(t)))
  def wrapperConstructorFunc(con: WrapperTypeCon): An[Func] = withTypeStructures(impl => An.valid(impl.wrapperConstructorFunc(con)))
  def casesCheck(piecewise: Ast.Piecewise, patterns: Seq[Terms.Pattern], matchedType: Option[Type]): An[Unit] =
    withTypeStructures(_.casesCheck(piecewise, patterns, matchedType))

  private def withTypeStructures[A](f: TypeInterpreterImpl => An[A]): An[A] = An.currentLog.flatMap(log => f(TypeInterpreterImpl(log.typeStructures)))

  def expandMethodTargets(topLevelTypeCon: TypeCon, pos: Pos): An[Set[TypeCon]] = {

    def methodTargets(con: TypeCon): An[Set[TypeCon]] = con match {
      case con @ WrapperTypeCon(TypeDefKind.Alias, _, _, _, _, _) => An.currentLog.flatMap { log =>
        aliasWrappedMethodTargets(log.typeStructures.structure(con)).map { wrappedTargets => Set(con) ++ wrappedTargets }
      }
      case con => An.valid(Set(con))
    }

    def aliasWrappedMethodTargets(wrappedType: Type): An[Set[TypeCons.TypeCon]] = wrappedType match {
      case typ: Nominal => methodTargets(typ.con)
      case Union(types) => An.seq(types.map {
        case t: Nominal => An.valid(t.con)
        case t => An.error(SourceMessage(pos, safe"Cannot have methods for type `${TypeCons.toSource(topLevelTypeCon)}` because it expands into a union type with non-nominal member type `${Types.toSource(t)}`"))
      }).map(_.toSet)
      // FIXME exhaustive
    }

    methodTargets(topLevelTypeCon)
  }
}

// A recursive type unwraps into itself, so each check that unwraps a tagged or boxed type remembers what it's doing
// (assumedAssignable, assumedOverlapping, expanding) while it checks the wrapped types, and reaching the same again
// ends there instead of looping. Each unwrapping step checks a copy with one more remembered, so they're empty for a
// new check. This terminates for regular recursion (a recursive reference has the type params of the con as its type
// args, so a finite number of types is reached) that is contractive (it's inside a tuple, a record or a function,
// so the head of a type unwraps into a non-wrapper in a finite number of steps).
case class TypeInterpreterImpl(
  typeStructures: TypeStructures,
  private val assumedAssignable: Set[(Type, Type)] = Set.empty,
  private val assumedOverlapping: Set[(Type, Type)] = Set.empty,
  // the cons and type args of the wrappers being expanded, the same for their expanded (narrowed) versions
  private val expanding: Set[(TypeConId, Seq[Type])] = Set.empty,
) {
  import NormalizeImpl._, ExpandUnionsImpl._, AssignableImpl._, DisjointImpl._

  def wrapperConstructorFunc(con: WrapperTypeCon): Func = {
    // TODO: is it safe to re-use type con's type params as type params for the func?
    Func(con.typeParams, typeStructures.structure(con), TypeCons.pseudoInstantiate(con))
  }

  def wrappedType(t: Wrapper): Type = t.expandedWrappedType.getOrElse(
    substituteParams(t.con.typeParams.map(_.uniq).zip(t.typeArgs).toMap, typeStructures.structure(t.con))
  )

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
    // Reached the same comparison again while comparing the wrapped types for it: assumed to hold, so the answer depends
    // only on the other parts of the comparison (coinductive, the usual for recursive types)
    case _ if assumedAssignable.contains((t1, t2)) => true
    // Alias needs to be unwrapped first
    case (t1: Alias, t2) => isAssignable(wrappedType(t1), t2)
    case (t1, t2: Alias) => isAssignable(t1, wrappedType(t2))
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
    case (t1: Tagged, t2: Tagged) => sameCon(t1, t2) && isAssignableWrapper(t1, t2)
    case (t1: Tagged, t2) => assumingAssignable(t1, t2).isAssignable(wrappedType(t1), t2)
    case (t1: Boxed, t2: Boxed) => sameCon(t1, t2) && isAssignableWrapper(t1, t2)
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
    // Reached the same comparison again while comparing the wrapped types for it: overlap can't be ruled out
    case _ if assumedOverlapping.contains((t1, t2)) => false
    case (t1: Alias, t2) => disjoint(wrappedType(t1), t2)
    case (t1, t2: Alias) => disjoint(t1, wrappedType(t2))
    case (Union(ts), t2) => ts.forall(disjoint(_, t2))
    case (t1, Union(ts)) => ts.forall(disjoint(t1, _))
    case (Intersection(ts), t2) => ts.exists(disjoint(_, t2))
    case (t1, Intersection(ts)) => ts.exists(disjoint(t1, _))
    case (t1: Tagged, t2) => assumingOverlapping(t1, t2).disjoint(wrappedType(t1), t2)
    case (t1, t2: Tagged) => assumingOverlapping(t1, t2).disjoint(t1, wrappedType(t2))
    case (Tuple(ts1), Tuple(ts2)) =>
      ts1.length != ts2.length || ts1.zip(ts2).exists { case (t1, t2) => disjoint(t1, t2) }
    case (_: Tuple, t2) => isDisjointFromTuples(t2)
    case (t1, _: Tuple) => isDisjointFromTuples(t1)
    // only shared fields matter, e.g. {a: Red} and {b: Blue} share the value {a: Red, b: Blue}
    case (Record(fields1), Record(fields2)) =>
      fields1.entries.exists { case (name, t1) => fields2.get(name).exists(disjoint(t1, _)) }
    case (t1: Boxed, t2: Boxed) =>
      !sameCon(t1, t2) || assumingOverlapping(t1, t2).disjoint(wrappedType(t1), wrappedType(t2))
    case (t1: Singleton, t2: Singleton) => !sameCon(t1, t2)
    case (_: Singleton, _: Boxed) | (_: Boxed, _: Singleton) => true
    case (t1: Opaque, t2: Opaque) => !sameCon(t1, t2)
    case (_: Opaque, _: Singleton | _: Boxed) | (_: Singleton | _: Boxed, _: Opaque) => true
    case _ => false
  }

  private def assumingAssignable(t1: Type, t2: Type) = copy(assumedAssignable = assumedAssignable + (t1 -> t2))
  private def assumingOverlapping(t1: Type, t2: Type) = copy(assumedOverlapping = assumedOverlapping + (t1 -> t2))

  def narrow(t1: Type, t2: Type): Type =
    if (isAssignable(t1, t2)) t1
    else if (isAssignable(t2, t1)) t2
    else Intersection(Seq(t1, t2))

  // Normalize type into tuple form. e.g. (Int, Bool) | (String, Bool) | Int gives (Int | String, Bool).
  // Types that can't be decomposed but could be such a tuple (e.g. Top or a type param) give Top.
  def tupleElementTypes(typ: Type, length: Int): Seq[Type] = typ match {
    case Tuple(ts) if ts.length == length => ts
    case t: Alias => tupleElementTypes(wrappedType(t), length)
    case t: Tagged => tupleElementTypes(wrappedType(t), length)
    case Union(ts) => ts.map(tupleElementTypes(_, length)).transpose.map(Union(_))
    case t: Intersection => normalizeHead(t) match {
      case Intersection(members) => members.map(tupleElementTypes(_, length)).transpose.map(Intersection(_))
      case normalized => tupleElementTypes(normalized, length)
    }
    case t if disjoint(t, Tuple(Seq.fill(length)(Top))) => Seq.fill(length)(Bottom)
    case _ => Seq.fill(length)(Top)
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
      case t: Alias => flattenIntersectionMembers(wrappedType(t))
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
        case t: Alias => expandUnions(wrappedType(t))
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
        // reached the same wrapper again while expanding its wrapped type: left unexpanded
        case t: Wrapper if expanding.contains((t.con.uniq, t.typeArgs)) => Some(Seq(t))
        case t: Tagged => expandWrappedType(t).map {
          // did not contain an union to expand so keep the original tagged type
          case Seq(_) => Seq(t)
          case unionMembers => unionMembers.map(unionMember => t.copy(expandedWrappedType = Some(unionMember)))
        }
        case t: Boxed => expandWrappedType(t).map {
          case Seq(_) => Seq(t)
          case unionMembers => unionMembers.map(unionMember => t.copy(expandedWrappedType = Some(unionMember)))
        }
        case t: Intersection => normalizeHead(t) match {
          case Intersection(members) => expandEach(members).map(_.map(Intersection(_)))
          case normalized => expandUnions(normalized)
        }
        case t => Some(Seq(t))
      }
    }

    def expandWrappedType(t: Wrapper): Option[Seq[Type]] =
      copy(expanding = expanding + (t.con.uniq -> t.typeArgs)).ExpandUnionsImpl.expandUnions(wrappedType(t))
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
    // needs no assumptions for recursive types since args are compared instead of expanded wrapped types.
    // t1 and t2 have the same con
    def isAssignableWrapper(t1: Wrapper, t2: Wrapper): Boolean = {
      val phantom = t1.con.typeParams.map(param => !mentionsParam(param.uniq, typeStructures.structure(t1.con)))
      def phantomArgs(typeArgs: Seq[Type]) = typeArgs.zip(phantom).collect { case (t, true) => t }

      isAssignableInvariantArgs(phantomArgs(t1.typeArgs), phantomArgs(t2.typeArgs)) &&
        assumingAssignable(t1, t2).isAssignable(wrappedType(t1), wrappedType(t2))
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
        case t: Wrapper => t.typeArgs.exists(mentions)
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

  // Every value of the matched type has to be covered by some case (error otherwise), and each case has to be able to
  // match some value earlier cases don't cover (warning otherwise). Without an expected type the matched type is what
  // the cases can match together, e.g. #(1 => ...) alone would be an Int => ... failing for other integers.
  // TODO: is this in right place because it depeneds on Terms instead of just Types
  def casesCheck(piecewise: Ast.Piecewise, patterns: Seq[Terms.Pattern], matchedType: Option[Type]): An[Unit] = {
    val covered = patterns.map(PatternAnalyzer.coveredType)

    val unreachableCases = patterns.zip(piecewise.pieces).zipWithIndex.collect {
      // TODO: could this be expressed somehow as a reduce so that we accumulate the covered types
      // and check reachability in one pass, instead of using take(i) for each case, for better performance?
      case ((pattern, (astPattern, _)), i) if isAssignable(pattern.typ, Types.Union(covered.take(i))) =>
        SourceMessage(astPattern.pos,
          safe"${PatternAnalyzer.quotedPattern(astPattern.pos)} is unreachable, earlier cases already match all its values"
        )
    }

    val matched = matchedType.getOrElse(Types.Union(patterns.map(_.typ)))

    // TODO remove when type patterns get runtime type tests (see construct patterns in TODO.md)
    val typeTestCases = patterns.zip(piecewise.pieces).collect {
      case (pattern, (astPattern, _)) if needsTypeTest(pattern, matched) =>
        An.error(SourceMessage(astPattern.pos,
          safe"${PatternAnalyzer.quotedPattern(astPattern.pos)} needs to test the type of the value at runtime, which isn't supported yet"
        ))
    }

    // TODO: show which values/patterns are not covered, i.e. which type/pattern is left uncovered
    val exhaustive = An.errorIf(!isAssignable(matched, Types.Union(covered)))(SourceMessage(piecewise.pos,
      safe"Cases of the piecewise function don't match every value of type `${Types.toSource(matched)}`, e.g. add a last case `_ => ...`"
    ))

    An.seq(typeTestCases :+ exhaustive).map(_ => ()).tellWarnings(unreachableCases)
  }

  // Whether matching the pattern against a value of matchedType needs a runtime type test, i.e. it has a type
  // pattern (p: T) whose type T narrower than the type of the value it's matched against
  def needsTypeTest(pattern: Terms.Pattern, matchedType: Type): Boolean = pattern match {
    case Terms.TuplePattern(ps, _) =>
      ps.zip(tupleElementTypes(matchedType, ps.length)).exists { case (p, typ) => needsTypeTest(p, typ) }
    case Terms.PatternAs(p, typ) => !isAssignable(matchedType, typ) || needsTypeTest(p, narrow(matchedType, typ))
    // a single capture matches every string, so it's a test for strings
    case Terms.StringPattern(Seq(_: TermLiteral.StringCapturePart), typ) => !isAssignable(matchedType, typ)
    // tuples are tested by the emitter, literals and named constants are compared by value
    case _: Terms.WildcardPattern | _: Terms.UnitPattern | _: Terms.IntegerPattern | _: Terms.FloatPattern |
      _: Terms.StringPattern | _: Terms.NamedConstantPattern | _: Terms.CapturePattern => false
  }
}

object TypeInterpreterImpl {
  def mismatchingTypeArgsMessage(paramCons: Seq[ParamCon], args: Seq[Type], con: TypeCon, pos: Pos) = {
    // FIXME better error message: print out params and args

    val message = if (paramCons.length === 0) {
      safe"Type `${TypeCons.toSource(con)}` does not take any type arguments, but ${args.length.toString} given"
    } else {
      safe"Type `${TypeCons.toSource(con)}` expects ${paramCons.length.toString} type arguments, but ${args.length.toString} given"
    }

    SourceMessage(pos, message)
  }

  def matchTypeArgs(paramCons: Seq[ParamCon], args: Seq[Type], con: TypeCon, pos: Pos): An[Map[TypeConId, Type]] = {
    if (paramCons.length === args.length) {
      An.valid(paramCons.map(_.uniq).zip(args).toMap)
    } else {
      An.error(mismatchingTypeArgsMessage(paramCons, args, con, pos))
    }
  }
}
