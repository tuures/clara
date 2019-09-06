package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.{Ast, Pos, SourceMessage}
import clara.util.Message


case class BlockAnalyzer(parentEnv: Env) {
  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Asg.Block] =
    bcs.zipWithIndex.foldLeft(WalkBlockState.begin) { case (state, (bc, index)) =>
      state.step(bc, index == bcs.length - 1)
    }.end.flatMap { case (contents, returnType, _) =>
      returnType match {
        case Some(typeInst) => An.result(Asg.Block(contents, typeInst, pos))
        case None => An.error(SourceMessage(pos, "Block must include an expression"))
      }
    }

  case class WalkBlockState(
    currentErrors: Vector[Message],
    currentLog: Vector[Message],
    currentContents: Vector[Asg.BlockContent],
    currentReturnType: Option[Asg.TypeInst],
    currentEnv: Env
  ) {
    def step(bc: Ast.BlockContent, isLast: Boolean): WalkBlockState = {
      walkBlockContent(currentContents, currentReturnType, currentEnv, bc, isLast) match {
        case An(Writer(Right((contents, returnType, nextEnv)), log)) => WalkBlockState(currentErrors, currentLog ++ log, contents, returnType, nextEnv)
        case An(Writer(Left(errors), log)) => WalkBlockState(currentErrors ++ errors, currentLog ++ log, currentContents, currentReturnType, currentEnv)
      }
    }
    def end = currentErrors.isEmpty match {
      case true => An(Writer(Right((currentContents, currentReturnType, currentEnv)), currentLog))
      case false => An(Writer(Left(currentErrors), currentLog))
    }
  }

  object WalkBlockState {
    def begin = WalkBlockState(Vector[Message](), Vector[Message](), Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentContents: Vector[Asg.BlockContent], currentReturnType: Option[Asg.TypeInst], currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[(Vector[Asg.BlockContent], Option[Asg.TypeInst], Env)] = {

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
      (currentContents :+ content, typeInst, nextEnv)
    }
  }

  def walkValueDef(currentEnv: Env, target: Ast.Pattern, t: Asg.TypeInst): An[Env] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, t), parentEnv, pos)
    case _ => ???
  }

}
