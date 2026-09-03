package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._

object BlockAnalyzerImpl {

  case class BlockState(
    currentEnv: Env,
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Type],
  ) {
    def unusedDefinitionWarnings(usageTrace: UsageTrace): Seq[SourceMessage] = {
      // TODO: Ignore exported definitions even when unused.
      val unusedValues = currentEnv.local.values.entries.collect {
        case (name, value) if !usageTrace.valueDefs.contains(value.uniq) =>
          SourceMessage(value.definedAt, safe"Unused value `$name`")
      }
      val unusedTypeCons = currentEnv.local.typeCons.entries.collect {
        case (name, typeCon) if !usageTrace.typeCons.contains(typeCon.uniq) =>
          SourceMessage(typeCon.definedAt, safe"Unused type `$name`")
      }

      (unusedValues ++ unusedTypeCons).sortBy(_.pos)
    }

    def buildTerm(blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] = currentReturnType match {
      case Some(typ) if isProgramBlock && (typ !== Types.Uni) => An.result(Terms.Block(currentContents, typ)).
        tellWarning(SourceMessage(blockPos, "Non-unit value discarded in program"))
      case Some(typ) => An.result(Terms.Block(currentContents, typ))
      case None if currentContents.isEmpty => An.result(Terms.Block(currentContents, Types.Uni))
      case None => An.result(Terms.Block(currentContents, Types.Uni)).
        tellWarning(SourceMessage(blockPos, "Block should end with an expression. Implicitly returning unit."))
    }
  }
  object BlockState {
    def begin(parentEnv: Env) = BlockState(parentEnv.startNestedScope, Nil.toVector, None)
  }

  case class BlockContentStep(nextEnv: Env, contentTerm: Terms.BlockContent, nextReturnType: Option[Types.Type])

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(currentEnv, valueExprTerm, Some(valueExprTerm.typ))).tellWarnings(maybeDiscardWarning)
      }
    case Ast.ValueDecl(name, t, pos) =>
      TypeExprAnalyzer.typeExprType(currentEnv, t).flatMap { typ =>
        // FIXME use namePos to be consistent with valueDef and typeDef
        currentEnv.addOrShadowValue(name, typ, pos)
      }.map { nextEnv =>
        BlockContentStep(nextEnv, Terms.ValueDecl(name), None)
      }
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, e).flatMap { valueExprTerm =>
        PatternAnalyzer(currentEnv).walkAssignment(target, Some(valueExprTerm.typ)).
          map { case (nextEnv, targetTerm) =>
            BlockContentStep(nextEnv, Terms.ValueDef(targetTerm, valueExprTerm), None)
          }
      }
    case typeDef: Ast.TypeDef =>
      TypeDefAnalyzer.typeDefTerm(currentEnv, typeDef).map { case (nextEnv, term) =>
        BlockContentStep(nextEnv, term, None)
      }
    case methodSection: Ast.MethodSection =>
      MethodSectionAnalyzer.methodSectionTerm(currentEnv, methodSection).
        map { case (nextEnv, term) => BlockContentStep(nextEnv, term, None) }
  }

  def walkBlockContents(parentEnv: Env, bcs: Seq[Ast.BlockContent]): An[BlockState] =
    An.step(bcs.zipWithIndex)(BlockState.begin(parentEnv)) { case (currentState, (bc, index)) =>
      val BlockState(currentEnv, currentContents, _) = currentState

      walkBlockContent(currentEnv, bc, index === bcs.length - 1).
        map { case BlockContentStep(nextEnv, contentTerm, nextReturnType) =>
          BlockState(nextEnv, currentContents :+ contentTerm, nextReturnType)
        }
    }

  def finishBlock(blockState: An[BlockState], blockPos: Pos, isProgramBlock: Boolean): An[Terms.Block] =
    blockState.zipLog.flatMap { case (blockState, log) =>
      An.result(blockState).tellWarnings(blockState.unusedDefinitionWarnings(log.usageTrace))
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
