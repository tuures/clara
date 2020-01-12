package clara.analyzer.impl

import clara.asg.Asg
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._
import clara.asg.Asg.StructuralTyp


case class BlockAnalyzer(parentEnv: Env) {

  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Asg.Block] =
    An.step(bcs.zipWithIndex)(WalkBlockState.begin) { case (currentState, (bc, index)) =>
      walkBlockContent(currentState, bc, index == bcs.length - 1)
    }.flatMap { case WalkBlockState(contents, returnType, _) =>
      returnType match {
        case Some(typ) => An.result(Asg.Block(contents, typ))
        case None => An.error(SourceMessage(pos, "Block must include an expression"))
      }
    }

  case class WalkBlockState(
    currentContents: Vector[Asg.BlockContent],
    currentReturnType: Option[Asg.Typ],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentState: WalkBlockState, bc: Ast.BlockContent, isLast: Boolean): An[WalkBlockState] = {
    val WalkBlockState(currentContents: Vector[Asg.BlockContent], currentReturnType: Option[Asg.Typ], currentEnv: Env) = currentState

    (bc match {
      case valueExprAst: Ast.ValueExpr => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprAsg =>
          val isUnit = valueExprAsg.typ === Asg.Uni

          lazy val discardWarning = SourceMessage(bc.pos, "Non-Unit value discarded in block")
          val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

          An.result((valueExprAsg, Some(valueExprAsg.typ), currentEnv)).tell(maybeDiscardWarning)
        }
      }
      case Ast.ValueNamesDef(target, e, _) => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprAsg =>
          walkValueNamesDef(currentEnv, target, valueExprAsg)
        }.map(nextEnv => (Asg.ValueNamesDef(), None, nextEnv))
      }
      case Ast.TypeDef(name, typeExpr, pos) => {
        TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap {
          case st: StructuralTyp =>
            currentEnv.addOrShadowType((name, Asg.Unique(st)), parentEnv, pos).
              map(nextEnv => (Asg.TypeDef(name), None, nextEnv))
          case _ => An.error(SourceMessage(typeExpr.pos, "Structural type expected"))
        }
      }
      case Ast.MethodSection(isDeclSection, typeName, methodAsts, pos) => {
        currentEnv.useType(typeName, pos /* FIXME: give more accurate Pos */).flatMap { typ =>
          walkMethods(currentEnv, isDeclSection, methodAsts).flatMap { methodsNs =>
            currentEnv.addMethods((typ, methodsNs), pos).map { nextEnv =>
              (Asg.MethodSection(typ), None, nextEnv)
            }
          }
        }
      }
    }).map { case (content, typ, nextEnv) =>
      WalkBlockState(currentContents :+ content, typ, nextEnv)
    }
  }

  def walkValueNamesDef(currentEnv: Env, target: Ast.Pattern, valueExprAsg: Asg.ValueExpr): An[Env] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, valueExprAsg.typ), parentEnv, pos)
    case _ => ???
  }

  def walkMethods(currentEnv: Env, isDeclSection: Boolean, methodAsts: Seq[Ast.Method]): An[Namespace[Asg.Typ]] = {
    An.step(methodAsts)(Namespace.empty[Asg.Typ]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDecl if !isDeclSection => An.error(SourceMessage(methodAst.pos, "Definition expected"))
        case _: Ast.MethodDef if isDeclSection => An.error(SourceMessage(methodAst.pos, "Declaration expected"))
        case Ast.MethodDecl(name, typeExpr, pos) =>
          TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap { typ =>
            lazy val error = SourceMessage(pos, "Already declared")

            An.someOrError(ns.add((name, typ)), error)
          }
        case Ast.MethodDef(name, body, pos) =>
          ValueExprAnalyzer(currentEnv).walkValueExpr(body).flatMap { valueExprAsg =>
            lazy val error = SourceMessage(pos, "Already defined")

            An.someOrError(ns.add((name, valueExprAsg.typ)), error)
          }
      })
    }
  }

}
