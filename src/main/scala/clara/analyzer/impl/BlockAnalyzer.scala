package clara.analyzer.impl

import clara.asg.{Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._


case class BlockAnalyzer(parentEnv: Env) {

  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Terms.Block] =
    An.step(bcs.zipWithIndex)(WalkBlockState.begin) { case (currentState, (bc, index)) =>
      walkBlockContent(currentState, bc, index == bcs.length - 1)
    }.flatMap { case WalkBlockState(contents, returnType, _) =>
      returnType match {
        case Some(typ) => An.result(Terms.Block(contents, typ))
        case None => An.error(SourceMessage(pos, "Block must end with an expression")) // TODO: make it a warning and return
      }
    }

  case class WalkBlockState(
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Typ],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentState: WalkBlockState, bc: Ast.BlockContent, isLast: Boolean): An[WalkBlockState] = {
    val WalkBlockState(currentContents: Vector[Terms.BlockContent], currentReturnType: Option[Types.Typ], currentEnv: Env) = currentState

    (bc match {
      case valueExprAst: Ast.ValueExpr => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprTerm =>
          val isUnit = valueExprTerm.typ === Types.Uni

          lazy val discardWarning = SourceMessage(bc.pos, "Non-Unit value discarded in block")
          val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

          An.result((valueExprTerm, Some(valueExprTerm.typ), currentEnv)).tell(maybeDiscardWarning)
        }
      }
      case Ast.ValueNamesDef(target, e, _) => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
          walkValueNamesDef(currentEnv, target, valueExprTerm)
        }.map { case (namesDef, nextEnv) => (namesDef, None, nextEnv) }
      }
      case Ast.TypeDef(name, typeExpr, pos) => {
        TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap {
          case st: Types.StructuralTyp =>
            currentEnv.addOrShadowType((name, Types.Unique(st)), parentEnv, pos).
              map(nextEnv => (Terms.TypeDef(name), None, nextEnv))
          case _ => An.error(SourceMessage(typeExpr.pos, "Structural type expected"))
        }
      }
      case Ast.MethodSection(isDeclSection, targetTypeName, methodAsts, pos) => {
        // TODO split into MethodSectionAnalyzer
        currentEnv.useType(targetTypeName, pos /* FIXME: give more accurate Pos */).flatMap { targetType =>
          (if (isDeclSection) {
            walkMethodDecls(currentEnv, methodAsts).map { methodDeclNs =>
              Terms.MethodDeclSection(targetType, methodDeclNs)
            }
          } else {
            walkMethodDefs(currentEnv, methodAsts).map { methodDefNs =>
              Terms.MethodDefSection(targetTypeName, targetType, methodDefNs)
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

  def walkValueNamesDef(currentEnv: Env, target: Ast.Pattern, valueExprTerm: Terms.ValueExpr): An[(Terms.ValueNamesDef, Env)] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, valueExprTerm.typ), parentEnv, pos).map { nextEnv =>
      (Terms.ValueNamesDef(Terms.NamePattern(name), valueExprTerm), nextEnv)
    }
    case _ => ???
  }

  def walkMethodDefs(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Terms.MethodDef]] = {
    An.step(methodAsts)(Namespace.empty[Terms.MethodDef]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
        case Ast.MethodDef(name, body, pos) =>
          ValueExprAnalyzer(currentEnv).walkValueExpr(body).flatMap { valueExprTerm =>
            lazy val error = SourceMessage(pos, "Already defined")

            An.someOrError(ns.add((name, Terms.MethodDef(valueExprTerm))), error)
          }
      })
    }
  }

  def walkMethodDecls(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Terms.MethodDecl]] = {
    An.step(methodAsts)(Namespace.empty[Terms.MethodDecl]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
        case Ast.MethodDecl(name, typeExpr, pos) =>
          TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap { typ =>
            lazy val error = SourceMessage(pos, "Already declared")

            An.someOrError(ns.add((name, Terms.MethodDecl(typ))), error)
          }
      })
    }
  }

}
