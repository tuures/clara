package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._
import clara.ast.Ast.ValueDecl


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
      case ValueDecl(name, t, pos) =>
        TypeExprAnalyzer(currentEnv).walkTypeExpr(t).flatMap { typ =>
          currentEnv.addOrShadowValue((name, typ), parentEnv, pos)
        }.map { nextEnv =>
          (Terms.ValueDecl(name), None, nextEnv)
        }
      case Ast.ValueNamesDef(target, e, _) => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
          walkValueNamesDef(currentEnv, target, valueExprTerm)
        }.map { case (namesDef, nextEnv) => (namesDef, None, nextEnv) }
      }
      // TODO remove duplication between alias and typedef
      case Ast.AliasTypeDef(name, typeParams, typeExpr, pos) => {
        TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
          TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
            Types.AliasCon(name, paramTypes, typ)
          }
        }.flatMap { aliasTypeCon =>
          currentEnv.addOrShadowType((name, aliasTypeCon), parentEnv, pos).
            map(nextEnv => (Terms.AliasTypeDef(name), None, nextEnv))
        }
      }
      case Ast.TypeDef(isDecl, name, typeParams, typeExpr, pos) => { // TODO maybe this should be called ::subtype or ::tagged instead, or ::uniq
        TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
          TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
            Types.UniqueCon(name, paramTypes, constructible = !isDecl, typ)
          }
        }.flatMap { uniqueTypeCon =>
          currentEnv.addOrShadowType((name, uniqueTypeCon), parentEnv, pos).
            map(nextEnv => (Terms.TypeDef(name), None, nextEnv))
        }
      }
      case Ast.MethodDeclSection(targetTypeExpr, methodAsts, pos) =>
        MethodSectionAnalyzer(currentEnv).walkDeclSection(targetTypeExpr, methodAsts, pos)
      case Ast.MethodDefSection(targetPattern, methodAsts, pos) =>
        MethodSectionAnalyzer(currentEnv).walkDefSection(targetPattern, methodAsts, pos)
    }).map { case (content, typ, nextEnv) =>
      WalkBlockState(currentContents :+ content, typ, nextEnv)
    }
  }

  def walkValueNamesDef(currentEnv: Env, target: Ast.Pattern, valueExprTerm: Terms.ValueExpr): An[(Terms.ValueNamesDef, Env)] = {
    PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, valueExprTerm.typ).map { case (targetTerm, nextEnv) =>
      (Terms.ValueNamesDef(targetTerm, valueExprTerm), nextEnv)
    }
  }

}

// FIXME move
case class TypeParamAnalyzer(env: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Seq[Types.Param], Env)] =
    An.step(typeParams)((Vector[Types.Param](), env)) { case ((currentParams, currentEnv), param) =>
      val Ast.TypeParam(name, pos) = param
      val paramTypeCon @ Types.ParamCon(_, uniq) = Types.ParamCon(name)
      // FIXME instantiated in two places, another in TypeAnalyzer.instantiate
      val paramType = Types.Param(name, uniq)

      currentEnv.addOrShadowType((name, paramTypeCon), env, pos).map { nextEnv =>
        (currentParams :+ paramType, nextEnv)
      }
    }
}
