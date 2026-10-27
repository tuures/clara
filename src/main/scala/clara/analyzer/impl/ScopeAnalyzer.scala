package clara.analyzer.impl

import clara.asg.{NsId, Terms, TypeConId, TypeCons, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

// A scope (a block or a namespace body) is analyzed in three passes, each over the whole scope before the next:
// 1. the names pass registers every name of the scope (type cons, namespaces, companions, values) without analyzing
//    any expression, and returns the terms of the type defs; a value is declared, the values pass defines it,
// 2. the types pass analyzes the type expressions (what each wrapper type wraps). The types pass also runs after errors
//    in the names pass (what failed to be registered is skipped) so that its errors are reported too, then the types of
//    the whole block (its namespaces included) are checked for recursion
// 3. the values pass analyzes the value expressions in source order (BlockAnalyzer).
// So a type can refer to any type of the scope, defined before or after it.
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
      ScopeAnalyzerImpl.rejectRecursion(typeDefTerms).map(_ => (env, typeDefTerms))
    }
  }

  // the namespace a namespace def registered in the names pass, None if it failed to be registered; an `::extend` finds
  // only a companion, a `::namespace` only a standalone namespace
  def registeredNamespace(env: Env, namespaceDef: Ast.NamespaceDef): Option[NsId] =
    if (namespaceDef.isExtend) env.localCompanionNamespace(namespaceDef.name.name)
    else env.localStandaloneNamespace(namespaceDef.name.name)
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
    // an `::extend`'s types are registered with its type, a later declaration of a name is a duplicate, values have no
    // types to register yet
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
          case EnvDeclaredValue(_, definedAt) => definedAt
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

  // TODO: recursive types: allow recursion through a nominal wrapper that is regular and contractive (TypeInterpreterImpl
  // terminates only for those, see Roadmap 2c)
  def rejectRecursion(typeDefTerms: Seq[Terms.BlockContent]): An[Unit] = An.currentLog.flatMap { log =>
    val typeStructures = log.typeStructures
    val cons = typeDefTerms.collect { case Terms.TypeDef(con: TypeCons.WrapperTypeCon) => con }
    val declared = cons.map(_.uniq).toSet

    def dependencies(con: TypeCons.WrapperTypeCon): Seq[TypeCons.WrapperTypeCon] =
      mentionedWrapperCons(typeStructures.structure(con)).filter(dependency => declared(dependency.uniq)).distinct

    // depth first, `path` holds the cons being visited, `done` the ones without a cycle
    def visit(done: Set[TypeConId], path: Vector[TypeCons.WrapperTypeCon], con: TypeCons.WrapperTypeCon): Either[SourceMessage, Set[TypeConId]] =
      if (done(con.uniq)) Right(done)
      else if (path.exists(_.uniq === con.uniq)) {
        val cycle = (path.dropWhile(_.uniq !== con.uniq) :+ con).map(c => safe"`${c.name}`").safeString(" -> ")
        Left(SourceMessage(con.definedAt, safe"Recursive types aren't supported yet: $cycle"))
      } else {
        dependencies(con).foldLeft[Either[SourceMessage, Set[TypeConId]]](Right(done)) { (either, dependency) =>
          either.flatMap(visit(_, path :+ con, dependency))
        }.map(_ + con.uniq)
      }

    cons.foldLeft[Either[SourceMessage, Set[TypeConId]]](Right(Set.empty)) { (either, con) =>
      either.flatMap(visit(_, Vector.empty, con))
    }.fold(An.error, _ => An.valid(()))
  }

  // the wrapper type cons a type refers to directly, without expanding them
  def mentionedWrapperCons(typ: Types.Type): Seq[TypeCons.WrapperTypeCon] = typ match {
    case t: Types.Wrapper => t.con +: t.typeArgs.flatMap(mentionedWrapperCons)
    case Types.Opaque(_, typeArgs) => typeArgs.flatMap(mentionedWrapperCons)
    case Types.Func(_, parameter, result) => mentionedWrapperCons(parameter) ++ mentionedWrapperCons(result)
    case Types.Record(fields) => fields.entries.flatMap { case (_, t) => mentionedWrapperCons(t) }
    case Types.Tuple(ts) => ts.flatMap(mentionedWrapperCons)
    case Types.Union(ts) => ts.flatMap(mentionedWrapperCons)
    case Types.Intersection(ts) => ts.flatMap(mentionedWrapperCons)
    case Types.Top | Types.Bottom | Types.Uni | _: Types.Param | _: Types.Singleton => Nil
  }
}
