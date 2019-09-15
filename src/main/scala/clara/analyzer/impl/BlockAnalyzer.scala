package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.{Ast, Pos, SourceMessage}
import clara.util.Message


case class BlockAnalyzer(parentEnv: Env) {

  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Asg.Block] =
    An.step(bcs.zipWithIndex)(WalkBlockState.begin) { case (currentState, (bc, index)) =>
      walkBlockContent(currentState, bc, index == bcs.length - 1)
    }.flatMap { case WalkBlockState(contents, returnType, _) =>
      returnType match {
        case Some(typeInst) => An.result(Asg.Block(contents, typeInst, pos))
        case None => An.error(SourceMessage(pos, "Block must include an expression"))
      }
    }

  case class WalkBlockState(
    currentContents: Vector[Asg.BlockContent],
    currentReturnType: Option[Asg.TypeInst],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentState: WalkBlockState, bc: Ast.BlockContent, isLast: Boolean): An[WalkBlockState] = {
    val WalkBlockState(currentContents: Vector[Asg.BlockContent], currentReturnType: Option[Asg.TypeInst], currentEnv: Env) = currentState

    (bc match {
      case e: Ast.ValueExpr => {
        /*val discardWarning = if (!isLast && !isUnit) Seq(SourceMessage(bc.pos, "Non-Unit value discarded in block")) else Nil*/
        /*.tell(discardWarning)*/
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).
          map(content => (content, Some(content.typeInst), currentEnv))
      }
      case Ast.ValueNamesDef(target, e, pos) => {
        // ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExpr =>
        //   walkValueDef(currentEnv, target, valueExpr.typeInst)
        // }
        //.map { (content, nextEnv) => (content, None, nextEnv) }
        ???
      }
      case Ast.TypeDef(name, pos) => {
        currentEnv.addType((name, Asg.DummyTypeCon()), pos).
          map(nextEnv => (Asg.TypeDef(name, pos), None, nextEnv))
      }
      // case classDef: Ast.ClassDef => walkClassDef(currentEnv)(classDef) map (newEnv => (newEnv, None))
    }).map { case (content, typeInst, nextEnv) =>
      WalkBlockState(currentContents :+ content, typeInst, nextEnv)
    }
  }

  def walkValueDef(currentEnv: Env, target: Ast.Pattern, t: Asg.TypeInst): An[Env] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, t), parentEnv, pos)
    case _ => ???
  }

}
