package clara.analyzer.impl

import clara.asg.{DefId, Terms, TypeCons, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._

// A scope (a block or a namespace body) is analyzed in three passes, each over the whole scope before the next:
// 1. NamesPass gives every declaration its name (type cons, namespaces, companions, values) without analyzing any
//    expression, so that declarations can refer to each other in any order. It returns the terms of the type defs,
//    which are hoisted to the top of the block so that singletons and constructors exist before any value uses them.
//    Only the first declaration of a name in a scope gets the name, a later one is a duplicate (the Env reports it)
//    and has no entry to act on in the later passes, see ScopeDecl. A type def whose type con fails stays declared,
//    holding its name.
// 2. TypesPass resolves the type expressions: what each wrapper type wraps, value def signatures (for recursive
//    functions), the types of `::declare`d values. Nothing is checked against them yet. It also runs after errors in
//    the names pass, so that its errors are reported too. If neither pass had errors, the types of the whole block
//    (its namespaces included) are checked for recursion (TypeInterpreter.rejectUnsupportedRecursion).
// 3. ValuesPass analyzes the value expressions in source order, checking types. It runs only if there were no errors
//    before it.
// A namespace body has no passes of its own: each pass of the scope it's in walks the body too, so that types inside
// and outside it can refer to each other. An error in a declaration loses only what the declaration would have done,
// the pass goes on (An.stepDeferring).
object BlockAnalyzerImpl {

  def unusedDefinitionWarnings(env: Env, usageTrace: UsageTrace): Seq[SourceMessage] = {
    // TODO: Ignore exported definitions even when unused, and the members of exported namespaces.
    def isUsed(defId: DefId) = usageTrace.defs.contains(defId)

    def unused(scope: BlockScope, nsPath: Seq[String]): Seq[(DefId, SourceMessage)] = {
      def qualified(name: String): String = (nsPath :+ name).safeString(".")

      val unusedTypes = scope.types.entries.flatMap {
        case (name, EnvTypeCon(con, defId)) if !isUsed(defId) =>
          Seq(defId -> SourceMessage(con.definedAt, safe"Unused type `${qualified(name)}`"))
        case (name, EnvCompanion(con, _, defId)) if !isUsed(defId) =>
          Seq(defId -> SourceMessage(con.definedAt, safe"Unused type `${qualified(name)}`"))
        case (name, EnvNamespace(_, defId, definedAt)) if !isUsed(defId) =>
          Seq(defId -> SourceMessage(definedAt, safe"Unused namespace `${qualified(name)}`"))
        case (_, _: EnvDeclaredType | _: EnvTypeCon) => Nil
        // an unused namespace (or a type with companion) is only reported alone if unused
        // if used, recurse to check its members
        case (name, EnvCompanion(_, nsId, _)) => unused(env.namespaceMembers(nsId), nsPath :+ name)
        case (name, EnvNamespace(nsId, _, _)) => unused(env.namespaceMembers(nsId), nsPath :+ name)
      }
      val unusedValues = scope.values.entries.collect {
        case (name, EnvValue(_, defId, _, definedAt)) if !isUsed(defId) =>
          defId -> SourceMessage(definedAt, safe"Unused value `${qualified(name)}`")
      }

      unusedTypes ++ unusedValues
    }

    // one warning per declaration, a declaration with both a type and a value (a singleton, a namespace) is reported
    // as a type
    unused(env.local, Nil).
      distinctBy { case (defId, _) => defId }.
      map { case (_, message) => message }.
      sortBy(_.pos)
  }

  def buildTerm(state: ValuesPass.State, blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] = {
    val ValuesPass.State(_, contents, returnType) = state

    returnType match {
      case Some(typ) if isProgramBlock && (typ !== Types.Uni) => An.valid(Terms.Block(contents, typ)).
        tellWarning(SourceMessage(blockPos, "Non-unit value discarded in program"))
      case Some(typ) => An.valid(Terms.Block(contents, typ))
      case None if contents.isEmpty => An.valid(Terms.Block(contents, Types.Uni))
      case None => An.valid(Terms.Block(contents, Types.Uni)).
        tellWarning(SourceMessage(blockPos, "Block should end with an expression. Implicitly returning unit."))
    }
  }

  // the names and types passes and the recursion check, the values pass comes after them
  def analyzeDeclarations(env: Env, bcs: Seq[Ast.BlockContent]): An[(Env, Vector[Terms.TypeDef])] = {
    val decls = ScopeDecl.of(bcs)

    An.escalateErrors(NamesPass.walk(env, decls).flatMap { case (namedEnv, typeDefTerms) =>
      TypesPass.walk(namedEnv, decls).map(typedEnv => (typedEnv, typeDefTerms))
    }).flatMap { case (env, typeDefTerms) =>
      val cons = typeDefTerms.map(_.con).collect { case con: TypeCons.WrapperTypeCon => con }

      TypeInterpreter.rejectUnsupportedRecursion(cons).map(_ => (env, typeDefTerms))
    }
  }

  def walkBlockContents(parentEnv: Env, bcs: Seq[Ast.BlockContent]): An[ValuesPass.State] =
    analyzeDeclarations(parentEnv.startNestedScope, bcs).flatMap { case (env, typeDefTerms) =>
      ValuesPass.walk(env, typeDefTerms, bcs)
    }

  def finishBlock(state: An[ValuesPass.State], blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] =
    state.flatMap { state =>
      An.currentLog.flatMap(log =>
        An.valid(state).tellWarnings(unusedDefinitionWarnings(state.env, log.usageTrace))
      )
    }.flatMap(buildTerm(_, blockPos, isProgramBlock))

  def blockTerm(parentEnv: Env, block: Ast.Block, isProgramBlock: Boolean): An[Terms.Block] =
    finishBlock(walkBlockContents(parentEnv, block.bcs), block.pos, isProgramBlock)

}

object BlockAnalyzer {
  def regularBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = false)

  def programBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = true)
}
