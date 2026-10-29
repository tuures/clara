package clara.analyzer.impl

import clara.asg.{NsId, Terms, TypeConId, TypeCons, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

import scala.annotation.tailrec

// A scope (a block or a namespace body) is analyzed in three passes, each over the whole scope before the next:
// 1. the names pass registers every name of the scope (type cons, namespaces, companions, values) without analyzing
//    any expression, and returns the terms of the type defs; a value is declared, a later pass defines it,
// 2. the types pass analyzes the type expressions (what each wrapper type wraps, the types of `::declare`d values) and
//    defines the `::declare`d values. The types pass also runs after errors in the names pass (what failed to be
//    registered is skipped) so that its errors are reported too, then the types of the whole block (its namespaces
//    included) are checked for recursion
// 3. the values pass analyzes the value expressions in source order (BlockAnalyzer) and defines the other values.
// So a type can refer to any type of the scope, defined before or after it, and so can a value to a `::declare`d value
// (it has no value expression to evaluate before it can be used).
//
// A namespace body has no passes of its own: each pass of the scope it's in walks the body too, so that types inside
// and outside it can refer to each other. An error in a declaration loses only what the declaration would have done,
// the pass goes on (An.stepDeferring).
//
// Only the first declaration of a name in a scope is registered (a later one is a duplicate), the later passes find
// what it registered by its name in the scope.
object ScopeAnalyzer {
  def registerNamesAndTypes(env: Env, bcs: Seq[Ast.BlockContent]): An[(Env, Vector[Terms.BlockContent])] = {
    val impl = ScopeAnalyzerImpl(bcs)

    An.escalateErrors(impl.registerNames(env).flatMap { case (namedEnv, typeDefTerms) =>
      impl.registerTypes(namedEnv).map(typedEnv => (typedEnv, typeDefTerms))
    }).flatMap { case (env, typeDefTerms) =>
      ScopeAnalyzerImpl.rejectUnsupportedRecursion(typeDefTerms).map(_ => (env, typeDefTerms))
    }
  }

  // the namespace a namespace def registered in the names pass, None if it failed to be registered; an `::extend` finds
  // only a companion, a `::namespace` only a standalone namespace
  def registeredNamespace(env: Env, namespaceDef: Ast.NamespaceDef): Option[NsId] =
    if (namespaceDef.isExtend) env.localCompanionNamespace(namespaceDef.name.name)
    else env.localStandaloneNamespace(namespaceDef.name.name)

  // the name and type of a value def whose target is a single annotated capture (`f: T = ...`): its type is known
  // before its value expression is analyzed. The annotations inside other patterns (e.g. a tuple's) are only checked
  // by the values pass.
  def topLevelSignature(target: Ast.Pattern): Option[(Ast.NameWithPos, Ast.TypeExpr)] = target match {
    case Ast.PatternAs(Ast.CapturePattern(name, pos), t, _) => Some((Ast.NameWithPos(name, pos), t))
    case _: Ast.WildcardPattern | _: Ast.UnitPattern | _: Ast.IntegerPattern | _: Ast.FloatPattern |
      _: Ast.StringPattern | _: Ast.TuplePattern | _: Ast.NamedConstantPattern | _: Ast.CapturePattern |
      _: Ast.GuardPattern | _: Ast.DefaultValuePattern | _: Ast.PatternAs | _: Ast.RecordPattern | _: Ast.OrPattern |
      _: Ast.ConstructPattern => None
  }

  // the signature the value def of this name got in the types pass (see topLevelSignature)
  def registeredSignature(env: Env, name: String): Types.Type = env.local.values.get(name) match {
    case Some(EnvDeclaredValue(_, Some(signature), _, _)) => signature
    case _ => throw new java.lang.AssertionError(safe"value `$name` hasn't been given a signature")
  }
}

case class ScopeAnalyzerImpl(bcs: Seq[Ast.BlockContent]) {
  import ScopeAnalyzerImpl._

  val contents: Vector[Ast.BlockContent] = bcs.toVector

  // whether each type level declaration is the first one of its name, a later one is a duplicate (the other contents
  // are true, the names of values are checked for duplicates when they're declared)
  val isFirst: Vector[Boolean] = contents.zip(contents.scanLeft(Set.empty[NameKey]) {
    case (keys, decl: Ast.TypeLevelDecl) => keys + nameKey(decl)
    case (keys, _) => keys
  }).map {
    case (decl: Ast.TypeLevelDecl, keysBefore) => !keysBefore(nameKey(decl))
    case _ => true
  }
  val firsts: Vector[Ast.TypeLevelDecl] = contents.zip(isFirst).collect { case (decl: Ast.TypeLevelDecl, true) => decl }

  val typeNames: Set[String] = firsts.collect { case typeDef: Ast.TypeDef => typeDef.target.name.name }.toSet
  val extendDefs: Map[String, Ast.NamespaceDef] = firsts.collect {
    case namespaceDef: Ast.NamespaceDef if namespaceDef.isExtend => namespaceDef.name.name -> namespaceDef
  }.toMap

  // the names pass, returns the terms of the type defs
  def registerNames(env: Env): An[(Env, Vector[Terms.BlockContent])] =
    An.stepDeferring(contents.zip(isFirst))((env, Vector.empty[Terms.BlockContent])) {
      case ((env, terms), (decl: Ast.TypeLevelDecl, isFirst)) =>
        registerName(env, decl, isFirst).map { case (nextEnv, declTerms) => (nextEnv, terms ++ declTerms) }
      case ((env, terms), (valueDef: Ast.ValueDef, _)) =>
        declareValues(env, PatternAnalyzer.boundNames(valueDef.target)).map(nextEnv => (nextEnv, terms))
      case ((env, terms), (valueDecl: Ast.ValueDecl, _)) => declareValues(env, Seq(valueDecl.name)).map(nextEnv => (nextEnv, terms))
      case ((env, terms), (_: Ast.ValueExpr | _: Ast.MethodSection, _)) => An.valid((env, terms))
    }

  def registerName(env: Env, decl: Ast.TypeLevelDecl, isFirst: Boolean): An[(Env, Seq[Terms.BlockContent])] = {
    val Ast.NameWithPos(name, pos) = nameOf(decl)

    decl match {
      case _ if isExtend(decl) && !typeNames(name) =>
        An.error(SourceMessage(pos, safe"Cannot extend `$name`, there's no type `$name` defined in this scope"))
      case _ if isExtend(decl) && !isFirst => An.error(SourceMessage(pos, safe"`$name` is already extended in this scope"))
      case _ if !isFirst => An.error(SourceMessage(pos, safe"Cannot shadow existing type with same name `$name`"))
      // registered with its type
      case _ if isExtend(decl) => An.valid((env, Nil))
      case typeDef: Ast.TypeDef => registerTypeDef(env, typeDef)
      // its body is walked also when its name fails to be registered
      case namespaceDef: Ast.NamespaceDef => An.deferErrors(env.addStandaloneNamespace(name, NsId(), pos))(env).
        flatMap(registerMemberNames(_, namespaceDef))
    }
  }

  // the type, then its companion and the names of the companion's members
  def registerTypeDef(env: Env, typeDef: Ast.TypeDef): An[(Env, Seq[Terms.BlockContent])] =
    TypeDefAnalyzer.typeCon(env, typeDef).flatMap { con =>
      TypeDefAnalyzer.registerName(env, typeDef, con).flatMap { envWithType =>
        companion(typeDef) match {
          case None => An.valid((envWithType, Nil))
          case Some(companion) =>
            An.deferErrors(registerCompanion(envWithType, con, companion))(envWithType).flatMap(registerMemberNames(_, companion))
        }
      }.map { case (nextEnv, memberTerms) => (nextEnv, Terms.TypeDef(con) +: memberTerms) }
    }

  def companion(typeDef: Ast.TypeDef): Option[Ast.NamespaceDef] =
    extendDefs.get(typeDef.target.name.name).orElse {
      // make sure there's a companion namespace for the constructor
      if (constructibleKind(typeDef)) Some(Ast.NamespaceDef(isExtend = true, typeDef.target.name, Nil, typeDef.pos)) else None
    }

  // the types pass, a companion's types after its type's
  def registerTypes(env: Env): An[Env] = An.stepDeferring(contents.zip(isFirst))(env) {
    case (env, (typeDef: Ast.TypeDef, true)) => An.deferErrors(registerType(env, typeDef))(env).flatMap { typedEnv =>
      companion(typeDef).fold(An.valid(typedEnv))(registerMemberTypes(typedEnv, _))
    }
    case (env, (namespaceDef: Ast.NamespaceDef, true)) if !namespaceDef.isExtend => registerMemberTypes(env, namespaceDef)
    case (env, (valueDef: Ast.ValueDef, _)) => registerSignature(env, valueDef)
    case (env, (valueDecl: Ast.ValueDecl, _)) => registerValueDeclType(env, valueDecl)
    // an `::extend`'s types are registered with its type, a later declaration of a name is a duplicate
    case (env, _) => An.valid(env)
  }
}

object ScopeAnalyzerImpl {
  def declareValues(env: Env, names: Seq[Ast.NameWithPos]): An[Env] =
    An.step(names)(env)((env, name) => env.declareValue(name.name, name.pos))

  def registerMemberNames(env: Env, namespaceDef: Ast.NamespaceDef): An[(Env, Seq[Terms.BlockContent])] =
    ScopeAnalyzerImpl(namespaceDef.body).registerNames(enterNamespace(env, namespaceDef)).map {
      case (bodyEnv, memberTerms) => (env.leaveNamespace(bodyEnv), memberTerms)
    }

  def registerMemberTypes(env: Env, namespaceDef: Ast.NamespaceDef): An[Env] =
    ScopeAnalyzerImpl(namespaceDef.body).registerTypes(enterNamespace(env, namespaceDef)).map(env.leaveNamespace)

  // a namespace that failed to be registered is entered detached (a namespace no name refers to), so its body's errors
  // are still reported but nothing the body registers is reachable
  def enterNamespace(env: Env, namespaceDef: Ast.NamespaceDef): Env =
    env.enterNamespace(namespaceDef.name.name, ScopeAnalyzer.registeredNamespace(env, namespaceDef).getOrElse(NsId()))

  def registerCompanion(env: Env, con: TypeCons.TypeCon, companion: Ast.NamespaceDef): An[Env] = con match {
    case _: TypeCons.SingletonTypeCon =>
      An.error(SourceMessage(companion.name.pos, safe"Extending a singleton isn't supported yet"))
    case con => env.addCompanionNamespace(con.name, NsId())
  }

  // what failed to be registered in the names pass isn't in the scope and is skipped
  def registerType(env: Env, typeDef: Ast.TypeDef): An[Env] = env.localTypeCon(typeDef.target.name.name) match {
    case Some(con: TypeCons.WrapperTypeCon) => registerWrapperTypes(env, typeDef, con)
    // opaque and singleton types wrap nothing
    case Some(_: TypeCons.OpaqueTypeCon | _: TypeCons.SingletonTypeCon) => An.valid(env)
    case Some(con: TypeCons.ParamCon) =>
      throw new java.lang.AssertionError(safe"type def `${con.name}` registered a type param")
    case None => An.valid(env)
  }

  def registerSignature(env: Env, valueDef: Ast.ValueDef): An[Env] =
    ScopeAnalyzer.topLevelSignature(valueDef.target) match {
      case None => An.valid(env)
      case Some((Ast.NameWithPos(name, _), t)) => TypeExprAnalyzer.typeExprType(env, t).map { typ =>
        env.local.values.get(name) match {
          case Some(EnvDeclaredValue(_, None, _, _)) => env.declareValueSignature(name, typ)
          case Some(EnvDeclaredValue(_, Some(_), _, _) | _: EnvValue | _: EnvNamespaceRef) | None => env
        }
      }
    }

  // FIXME:
  // a `::declare`d value is defined by its type. A duplicate (reported in the names pass) finds the name of another
  // declaration: skipped if it's defined already, defined if it's an earlier value def's (unseen, the values pass doesn't
  // run after the error), until a DefId per declaration (see the error type TODO)
  def registerValueDeclType(env: Env, valueDecl: Ast.ValueDecl): An[Env] = {
    val Ast.NameWithPos(name, pos) = valueDecl.name

    TypeExprAnalyzer.typeExprType(env, valueDecl.t).flatMap { typ =>
      env.local.values.get(name) match {
        case Some(_: EnvDeclaredValue) => env.defineValue(name, typ, pos)
        case Some(_: EnvValue | _: EnvNamespaceRef) | None => An.valid(env)
      }
    }
  }

  def registerWrapperTypes(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Env] =
    TypeDefAnalyzer.registerStructure(env, typeDef, con).flatMap { _ =>
      if (!constructibleKind(typeDef)) An.valid(env)
      else rejectOwnApply(env, con).flatMap(_ => TypeInterpreter.wrapperConstructorFunc(con)).map { constructorType =>
        env.addConstructor(con.name, con, constructorType, typeDef.target.name.pos)
      }
    }

  // the companion's `apply` is the type's constructor, a member of the same name is an error until `::new`
  def rejectOwnApply(env: Env, con: TypeCons.WrapperTypeCon): An[Unit] =
    env.localCompanionNamespace(con.name).flatMap(env.namespaceMembers(_).values.get(TypeCons.ConstructorName)) match {
      case None => An.valid(())
      case Some(member) =>
        val pos = member match {
          case EnvValue(_, _, _, definedAt) => definedAt
          case EnvDeclaredValue(_, _, _, definedAt) => definedAt
          case _: EnvNamespaceRef => con.definedAt
        }

        An.error(SourceMessage(pos, safe"`${TypeCons.ConstructorName}` of `${con.name}` is its constructor, it can't be redefined yet"))
    }

  // a type and its companion (`::extend`) have the same name but count separately
  type NameKey = (Boolean, String)
  def nameKey(decl: Ast.TypeLevelDecl): NameKey = (isExtend(decl), nameOf(decl).name)

  def isExtend(decl: Ast.TypeLevelDecl): Boolean = decl match {
    case namespaceDef: Ast.NamespaceDef => namespaceDef.isExtend
    case _: Ast.TypeDef => false
  }

  def nameOf(decl: Ast.TypeLevelDecl): Ast.NameWithPos = decl match {
    case typeDef: Ast.TypeDef => typeDef.target.name
    case namespaceDef: Ast.NamespaceDef => namespaceDef.name
  }

  // FIXME: belongs in Types?
  def constructibleKind(typeDef: Ast.TypeDef): Boolean = typeDef.typeDefKind match {
    case Ast.TypeDefKind.Tagged | Ast.TypeDefKind.Boxed => true
    case _ => false
  }

  // A recursive type (type defs referring to each other) is supported when TypeInterpreterImpl terminates on it:
  // - every cycle goes through a tagged or boxed type: unwrapping an alias isn't remembered
  // - every cycle is contractive, i.e. goes through a tuple, a record, a function or a boxed type: otherwise the
  //   type refers to itself directly and its head unwraps forever
  // - the recursion is regular, i.e. a recursive reference has type params of the referring type as its type args:
  //   so that unwrapping reaches a finite number of types
  // Each group of type defs that are in cycles with each other (a strongly connected component) is checked on its
  // own, with an error for each rule it breaks.
  def rejectUnsupportedRecursion(typeDefTerms: Seq[Terms.BlockContent]): An[Unit] = An.currentLog.flatMap { log =>
    val cons = typeDefTerms.collect { case Terms.TypeDef(con: TypeCons.WrapperTypeCon) => con }
    val conIds = cons.map(_.uniq).toSet

    val referencesFrom: Map[TypeConId, Seq[Reference]] = cons.map { con =>
      con.uniq -> references(log.typeStructures.structure(con), guarded = false).filter(r => conIds(r.to.con.uniq))
    }.toMap

    // the cons reachable through the references of con, con itself only through a cycle
    def reachable(con: TypeCons.WrapperTypeCon): Set[TypeConId] = {
      @tailrec
      def loop(toVisit: List[TypeConId], seen: Set[TypeConId]): Set[TypeConId] = toVisit match {
        case Nil => seen
        case next :: rest =>
          val unseen = referencesFrom(next).map(_.to.con.uniq).distinct.filterNot(seen)
          loop(unseen.toList ++ rest, seen ++ unseen)
      }
      loop(List(con.uniq), Set.empty)
    }
    val reachableFrom = cons.map(con => con.uniq -> reachable(con)).toMap

    val components = cons.filter(con => reachableFrom(con.uniq)(con.uniq)).map { con =>
      cons.filter(other => reachableFrom(con.uniq)(other.uniq) && reachableFrom(other.uniq)(con.uniq))
    }.distinct

    An.seq(components.map(rejectUnsupportedComponent(_, referencesFrom))).map(_ => ())
  }

  def rejectUnsupportedComponent(
    component: Seq[TypeCons.WrapperTypeCon],
    referencesFrom: Map[TypeConId, Seq[Reference]],
  ): An[Unit] = {
    def cycleText(cycle: Seq[TypeCons.WrapperTypeCon]) = cycle.map(c => safe"`${c.name}`").safeString(" -> ")

    val aliasCycleError =
      findCycle(component.filter(_.typeDefKind == Ast.TypeDefKind.Alias), referencesFrom, _ => true).map { cycle =>
        SourceMessage(cycle.head.definedAt,
          safe"Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: ${cycleText(cycle)}"
        )
      }
    val uncontractiveCycleError =
      findCycle(component.filterNot(_.typeDefKind == Ast.TypeDefKind.Boxed), referencesFrom, !_.guarded).map { cycle =>
        SourceMessage(cycle.head.definedAt,
          safe"A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: ${cycleText(cycle)}"
        )
      }
    val irregularReferenceErrors = component.flatMap { con =>
      referencesFrom(con.uniq).
        filter(r => component.contains(r.to.con)).
        // each type argument of the referred type must be a plain type parameter of the referring type
        filterNot(r => r.to.typeArgs.forall(arg => con.typeParams.exists(arg == Types.Param(_)))).
        map { r =>
          SourceMessage(con.definedAt,
            safe"A recursive type can only refer to itself with type parameters as type arguments, not `${Types.toSource(r.to)}`"
          )
        }
    }

    An.seq((aliasCycleError.toSeq ++ uncontractiveCycleError ++ irregularReferenceErrors).map(An.error)).map(_ => ())
  }

  // a cycle among cons through the references `follow` lets through, depth first
  def findCycle(
    cons: Seq[TypeCons.WrapperTypeCon],
    referencesFrom: Map[TypeConId, Seq[Reference]],
    follow: Reference => Boolean,
  ): Option[Vector[TypeCons.WrapperTypeCon]] = {
    type Cycle = Vector[TypeCons.WrapperTypeCon]
    // a cycle found, or the cons visited without one
    type Visited = Either[Cycle, Set[TypeConId]]

    val conIds = cons.map(_.uniq).toSet
    def next(con: TypeCons.WrapperTypeCon) = referencesFrom(con.uniq).
      filter(reference => follow(reference) && conIds(reference.to.con.uniq)).map(_.to.con).distinct

    // `path` holds the cons being visited, `done` the ones without a cycle
    def visit(done: Set[TypeConId], path: Cycle, con: TypeCons.WrapperTypeCon): Visited =
      if (done(con.uniq)) Right(done)
      else if (path.exists(_.uniq === con.uniq)) Left(path.dropWhile(_.uniq !== con.uniq) :+ con)
      else next(con).foldLeft[Visited](Right(done)) { (visited, nextCon) =>
        visited.flatMap(visit(_, path :+ con, nextCon))
      }.map(_ + con.uniq)

    val nothingVisited: Visited = Right(Set.empty)
    val allVisited = cons.foldLeft(nothingVisited) { (visited, con) =>
      visited.flatMap(visit(_, Vector.empty, con))
    }

    allVisited match {
      case Left(cycle) => Some(cycle)
      case Right(_) => None
    }
  }

  // a reference to a named type in a type
  // guarded if it's inside a tuple, a record, a function or the type args of a boxed or opaque type
  case class Reference(to: Types.Wrapper, guarded: Boolean)

  // find all references to named types (which can contain recursion) in a type's structure, without expanding them
  def references(structure: Types.Type, guarded: Boolean): Seq[Reference] = structure match {
    case t: Types.Boxed => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded = true))
    case t: Types.Alias => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded))
    case t: Types.Tagged => Reference(t, guarded) +: t.typeArgs.flatMap(references(_, guarded))
    case Types.Opaque(_, typeArgs) => typeArgs.flatMap(references(_, guarded = true))
    case Types.Func(_, parameter, result) => references(parameter, guarded = true) ++ references(result, guarded = true)
    case Types.Record(fields) => fields.entries.flatMap { case (_, t) => references(t, guarded = true) }
    case Types.Tuple(ts) => ts.flatMap(references(_, guarded = true))
    case Types.Union(ts) => ts.flatMap(references(_, guarded))
    case Types.Intersection(ts) => ts.flatMap(references(_, guarded))
    case Types.Top | Types.Bottom | Types.Uni | _: Types.Param | _: Types.Singleton => Nil
  }
}
