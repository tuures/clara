package clara.analyzer.impl

import clara.asg.{Asg, Namespace}
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
        case None => An.error(SourceMessage(pos, "Block must end with an expression")) // TODO: make it a warning and return
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
        }.map { case (namesDef, nextEnv) => (namesDef, None, nextEnv) }
      }
      case Ast.TypeDef(name, typeExpr, pos) => {
        TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap {
          case st: StructuralTyp =>
            currentEnv.addOrShadowType((name, Asg.Unique(st)), parentEnv, pos).
              map(nextEnv => (Asg.TypeDef(name), None, nextEnv))
          case _ => An.error(SourceMessage(typeExpr.pos, "Structural type expected"))
        }
      }
      case Ast.MethodSection(isDeclSection, targetTypeName, methodAsts, pos) => {
        // TODO split into MethodSectionAnalyzer
        currentEnv.useType(targetTypeName, pos /* FIXME: give more accurate Pos */).flatMap { targetType =>
          (if (isDeclSection) {
            walkMethodDecls(currentEnv, methodAsts).map { methodDeclNs =>
              Asg.MethodDeclSection(targetType, methodDeclNs)
            }
          } else {
            walkMethodDefs(currentEnv, methodAsts).map { methodDefNs =>
              Asg.MethodDefSection(targetTypeName, targetType, methodDefNs)
            }
          }).flatMap { methodSection =>
            currentEnv.addMethods((targetType, methodSection), pos).map { nextEnv =>
              (methodSection, None, nextEnv)
            }
          }
        }
      }
    }).map { case (content, typ, nextEnv) =>
      WalkBlockState(currentContents :+ content, typ, nextEnv)
    }
  }

  def walkValueNamesDef(currentEnv: Env, target: Ast.Pattern, valueExprAsg: Asg.ValueExpr): An[(Asg.ValueNamesDef, Env)] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, valueExprAsg.typ), parentEnv, pos).map { nextEnv =>
      (Asg.ValueNamesDef(Asg.NamePattern(name), valueExprAsg), nextEnv)
    }
    case _ => ???
  }

  def walkMethodDefs(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Asg.MethodDef]] = {
    An.step(methodAsts)(Namespace.empty[Asg.MethodDef]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
        case Ast.MethodDef(name, body, pos) =>
          ValueExprAnalyzer(currentEnv).walkValueExpr(body).flatMap { valueExprAsg =>
            lazy val error = SourceMessage(pos, "Already defined")

            An.someOrError(ns.add((name, Asg.MethodDef(valueExprAsg))), error)
          }
      })
    }
  }

  def walkMethodDecls(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Asg.MethodDecl]] = {
    An.step(methodAsts)(Namespace.empty[Asg.MethodDecl]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
        case Ast.MethodDecl(name, typeExpr, pos) =>
          TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap { typ =>
            lazy val error = SourceMessage(pos, "Already declared")

            An.someOrError(ns.add((name, Asg.MethodDecl(typ))), error)
          }
      })
    }
  }

}
