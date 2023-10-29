package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class BlockAnalyzerState(
  currentEnv: Env,
  currentContents: Vector[Terms.BlockContent],
  currentReturnType: Option[Types.Type],
) {
  def finishTerm(blockPos: Pos): An[Terms.Block] = currentReturnType match {
    case Some(typ) => An.result(Terms.Block(currentContents, typ))
    case None => An.result(Terms.Block(currentContents, Types.Uni)).
      tell(SourceMessage(blockPos, "Block should end with an expression."))
  }
}
object BlockAnalyzerState {
  def begin(parentEnv: Env) = BlockAnalyzerState(parentEnv, Nil.toVector, None)
}

case class BlockAnalyzerImpl(parentEnv: Env) {

  case class BlockContentStep(nextEnv: Env, contentTerm: Terms.BlockContent, nextReturnType: Option[Types.Type])

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(currentEnv, valueExprTerm, Some(valueExprTerm.typ))).tell(maybeDiscardWarning)
      }
    case Ast.ValueDecl(name, t, pos) =>
      TypeExprAnalyzer.typeExprType(currentEnv, t).flatMap { typ =>
        // FIXME use namePos to be consistent with valueDef and typeDef
        currentEnv.addOrShadowValue((name, typ), parentEnv, pos)
      }.map { nextEnv =>
        BlockContentStep(nextEnv, Terms.ValueDecl(name), None)
      }
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer.valueExprTerm(currentEnv, e).flatMap { valueExprTerm =>
        PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, Some(valueExprTerm.typ)).
          map { case (nextEnv, targetTerm) =>
            BlockContentStep(nextEnv, Terms.ValueDef(targetTerm, valueExprTerm), None)
          }
      }
    case typeDef: Ast.TypeDef =>
      TypeDefAnalyzer.typeDefTerm(currentEnv, parentEnv, typeDef).map { case (nextEnv, typeDefTerm) =>
        BlockContentStep(nextEnv, typeDefTerm, None)
    }
    case Ast.MethodDeclSection(targetTypeName, methods, _) => ???
      // MethodSectionAnalyzer(currentEnv).walkDeclSection(targetTypeName, methods)
      //   .map { case (term, nextEnv) => (term, None, nextEnv) }
    case Ast.MethodDefSection(targetTypeName, selfPattern, methods, _) => ???
      // MethodSectionAnalyzer(currentEnv).walkDefSection(targetTypeName, selfPattern, methods)
      //   .map { case (term, nextEnv) => (term, None, nextEnv) }
  }

  def walkBlockContents(bcs: Seq[Ast.BlockContent]): An[BlockAnalyzerState] =
    An.step(bcs.zipWithIndex)(BlockAnalyzerState.begin(parentEnv)) { case (currentState, (bc, index)) =>
      val BlockAnalyzerState(currentEnv, currentContents, _) = currentState

      walkBlockContent(currentEnv, bc, index === bcs.length - 1).
        map { case BlockContentStep(nextEnv, contentTerm, nextReturnType) =>
          BlockAnalyzerState(nextEnv, currentContents :+ contentTerm, nextReturnType)
        }
    }

  def blockTerm(block: Ast.Block): An[Terms.Block] = walkBlockContents(block.bcs).flatMap(_.finishTerm(block.pos))
}

object BlockAnalyzer {
  def blockTerm(parentEnv: Env, block: Ast.Block): An[Terms.Block] = BlockAnalyzerImpl(parentEnv).blockTerm(block)
}
