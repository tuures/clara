package clara.analyzer.impl

import clara.asg.{Terms, Types}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._
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
    currentReturnType: Option[Types.MonoType],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentState: WalkBlockState, bc: Ast.BlockContent, isLast: Boolean): An[WalkBlockState] = {
    val WalkBlockState(currentContents: Vector[Terms.BlockContent], currentReturnType: Option[Types.Type], currentEnv: Env) = currentState

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
      case Ast.ValueDef(target, e, _) => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
          walkValueDef(currentEnv, target, valueExprTerm)
        }.map { case (namesDef, nextEnv) => (namesDef, None, nextEnv) }
      }
      // TODO remove duplication between alias and typedef
      case Ast.AliasTypeDef(name, typeParams, typeExpr, pos) => {
        TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
          TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
            Types.maybeForAll(paramTypes, Types.Alias(name, typ))
          }
        }.flatMap { aliasType =>
          currentEnv.addOrShadowType((name, aliasType), parentEnv, pos).
            map(nextEnv => (Terms.AliasTypeDef(name), None, nextEnv))
        }
      }
      case Ast.TypeDef(isDecl, name, typeParams, typeExpr, pos) => {
        // TODO maybe this should be called ::tagged instead
        TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
          TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
            Types.maybeForAll(paramTypes, Types.Alias(name, Types.Unique(constructible = !isDecl, typ)))
          }
        }.flatMap { uniqueType =>
          currentEnv.addOrShadowType((name, uniqueType), parentEnv, pos).
            map(nextEnv => (Terms.TypeDef(name), None, nextEnv))
        }
      }
      case Ast.MethodDeclSection(targetTypeName, methods, _) =>
        MethodSectionAnalyzer(currentEnv).walkDeclSection(targetTypeName, methods)
          .map { case (term, nextEnv) => (term, None, nextEnv) }
      case Ast.MethodDefSection(targetTypeName, selfPattern, methods, _) =>
        MethodSectionAnalyzer(currentEnv).walkDefSection(targetTypeName, selfPattern, methods)
          .map { case (term, nextEnv) => (term, None, nextEnv) }
    }).map { case (content, typ, nextEnv) =>
      WalkBlockState(currentContents :+ content, typ, nextEnv)
    }
  }

  def walkValueDef(currentEnv: Env, target: Ast.Pattern, valueExprTerm: Terms.ValueExpr): An[(Terms.ValueDef, Env)] = {
    PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, valueExprTerm.typ).map { case (targetTerm, nextEnv) =>
      (Terms.ValueDef(targetTerm, valueExprTerm), nextEnv)
    }
  }

}

// FIXME move
case class TypeParamAnalyzer(env: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Seq[Types.Param], Env)] =
    An.step(typeParams)((Vector[Types.Param](), env)) { case ((currentParams, currentEnv), Ast.TypeParam(name, pos)) =>
      val paramType = Types.Param(name)

      currentEnv.addOrShadowType((name, paramType), env, pos).map { nextEnv =>
        (currentParams :+ paramType, nextEnv)
      }
    }
}
