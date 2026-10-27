package clara.analyzer.impl

import clara.asg.{DefId, Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._

object BlockAnalyzerImpl {

  case class BlockState(
    currentEnv: Env,
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Type],
  ) {
    def unusedDefinitionWarnings(usageTrace: UsageTrace): Seq[SourceMessage] = {
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
          case (_, _: EnvTypeCon) => Nil
          // an unused namespace (or a type with companion) is only reported alone if unused
          // if used, recurse to check its members
          case (name, EnvCompanion(_, nsId, _)) => unused(currentEnv.namespaceMembers(nsId), nsPath :+ name)
          case (name, EnvNamespace(nsId, _, _)) => unused(currentEnv.namespaceMembers(nsId), nsPath :+ name)
        }
        val unusedValues = scope.values.entries.collect {
          case (name, EnvValue(_, defId, _, definedAt)) if !isUsed(defId) =>
            defId -> SourceMessage(definedAt, safe"Unused value `${qualified(name)}`")
        }

        unusedTypes ++ unusedValues
      }

      // one warning per declaration, a declaration with both a type and a value (a singleton, a namespace) is reported
      // as a type
      unused(currentEnv.local, Nil).
        distinctBy { case (defId, _) => defId }.
        map { case (_, message) => message }.
        sortBy(_.pos)
    }

    def buildTerm(blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] = currentReturnType match {
      case Some(typ) if isProgramBlock && (typ !== Types.Uni) => An.valid(Terms.Block(currentContents, typ)).
        tellWarning(SourceMessage(blockPos, "Non-unit value discarded in program"))
      case Some(typ) => An.valid(Terms.Block(currentContents, typ))
      case None if currentContents.isEmpty => An.valid(Terms.Block(currentContents, Types.Uni))
      case None => An.valid(Terms.Block(currentContents, Types.Uni)).
        tellWarning(SourceMessage(blockPos, "Block should end with an expression. Implicitly returning unit."))
    }
  }

  case class BlockContentStep(nextEnv: Env, contentTerms: Seq[Terms.BlockContent], nextReturnType: Option[Types.Type])

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.valid(BlockContentStep(currentEnv, Seq(valueExprTerm), Some(valueExprTerm.typ))).tellWarnings(maybeDiscardWarning)
      }
    // defined by the types pass
    case Ast.ValueDecl(Ast.NameWithPos(name, _), _, _) => An.valid(BlockContentStep(currentEnv, Seq(Terms.ValueDecl(name)), None))
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, e).flatMap { valueExprTerm =>
        PatternAnalyzer.walkValueDef(currentEnv, target, Some(valueExprTerm.typ)).
          map { case (nextEnv, targetTerm) =>
            BlockContentStep(nextEnv, Seq(Terms.ValueDef(targetTerm, currentEnv.nsPath, valueExprTerm)), None)
          }
      }
    // type defs have already been analyzed by the names and types passes, but here affects the block's return value
    // (block should end with an expression)
    case _: Ast.TypeDef => An.valid(BlockContentStep(currentEnv, Nil, None))
    // the namespace's values become the block's own value defs (they know their namespace)
    // an error in one doesn't lose the others
    case namespaceDef: Ast.NamespaceDef =>
      // the values pass of a scope runs only when its names and types passes had no errors
      val nsId = ScopeAnalyzer.registeredNamespace(currentEnv, namespaceDef).getOrElse(
        throw new java.lang.AssertionError(safe"namespace `${namespaceDef.name.name}` hasn't been registered")
      )
      val bodyEnv = currentEnv.enterNamespace(namespaceDef.name.name, nsId)

      An.stepDeferring(namespaceDef.body)((bodyEnv, Vector.empty[Terms.BlockContent])) { case ((bodyEnv, terms), member) =>
        walkBlockContent(bodyEnv, member, isLast = false).map(step => (step.nextEnv, terms ++ step.contentTerms))
      }.map { case (bodyEnv, terms) => BlockContentStep(currentEnv.leaveNamespace(bodyEnv), terms, None) }
    case methodSection: Ast.MethodSection =>
      MethodSectionAnalyzer.methodSectionTerm(currentEnv, methodSection).
        map { case (nextEnv, term) => BlockContentStep(nextEnv, Seq(term), None) }
  }

  def walkBlockContents(parentEnv: Env, bcs: Seq[Ast.BlockContent]): An[BlockState] = {
    val typesAn = ScopeAnalyzer.registerNamesAndTypes(parentEnv.startNestedScope, bcs).map { case (env, typeDefTerms) =>
      BlockState(env, typeDefTerms, None)
    }

    typesAn.flatMap { stateWithTypeDefs =>
      An.step(bcs.zipWithIndex)(stateWithTypeDefs) { case (currentState, (bc, index)) =>
        val BlockState(currentEnv, currentContents, _) = currentState

        walkBlockContent(currentEnv, bc, index === bcs.length - 1).
          map { case BlockContentStep(nextEnv, contentTerms, nextReturnType) =>
            BlockState(nextEnv, currentContents ++ contentTerms, nextReturnType)
          }
      }
    }
  }

  def finishBlock(blockState: An[BlockState], blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] =
    blockState.flatMap { blockState =>
      An.currentLog.flatMap(log =>
        An.valid(blockState).tellWarnings(blockState.unusedDefinitionWarnings(log.usageTrace))
      )
    }.flatMap(_.buildTerm(blockPos, isProgramBlock))

  def blockTerm(parentEnv: Env, block: Ast.Block, isProgramBlock: Boolean): An[Terms.Block] =
    finishBlock(walkBlockContents(parentEnv, block.bcs), block.pos, isProgramBlock)

}

object BlockAnalyzer {
  def regularBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = false)

  def programBlockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] =
    BlockAnalyzerImpl.blockTerm(parentEnv, block, isProgramBlock = true)
}
