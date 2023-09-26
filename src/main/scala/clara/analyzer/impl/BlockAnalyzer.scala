package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._
import clara.ast.Ast.ValueDecl


case class BlockAnalyzer(parentEnv: Env) {

  case class WalkBlockState(
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Type],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  case class BlockContentStep(contentTerm: Terms.BlockContent, nextReturnType: Option[Types.Type], nextEnv: Env)

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr => {
      ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(valueExprTerm, Some(valueExprTerm.typ), currentEnv)).tell(maybeDiscardWarning)
      }
    }
    case ValueDecl(name, t, pos) =>
      TypeExprAnalyzer(currentEnv).walkTypeExpr(t).flatMap { typ =>
        currentEnv.addOrShadowValue((name, typ), parentEnv, pos)
      }.map { nextEnv =>
        BlockContentStep(Terms.ValueDecl(name), None, nextEnv)
      }
    case Ast.ValueDef(target, e, _) => {
      ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
        walkValueDef(currentEnv, target, valueExprTerm)
      }.map { case (namesDef, nextEnv) => BlockContentStep(namesDef, None, nextEnv) }
    }
    // TODO remove duplication between alias and typedef
    // case Ast.AliasTypeDef(name, typeParams, typeExpr, pos) => {
    //   TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
    //     TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
    //       Types.maybeForAll(paramTypes, Types.Alias(name, typ))
    //     }
    //   }.flatMap { aliasType =>
    //     currentEnv.addOrShadowType((name, aliasType), parentEnv, pos).
    //       map(nextEnv => (Terms.AliasTypeDef(name), None, nextEnv))
    //   }
    // }
    // case Ast.TypeDef(typeDefKind, name, typeParams, typeExpr, pos) => {
    //   TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
    //     TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
    //       Types.maybeForAll(paramTypes, Types.Alias(name, Types.Unique(constructible = !isDecl, typ)))
    //     }
    //   }.flatMap { uniqueType =>
    //     currentEnv.addOrShadowType((name, uniqueType), parentEnv, pos).
    //       map(nextEnv => (Terms.TypeDef(name), None, nextEnv))
    //   }
    // }
    case Ast.TypeDef(typeDefKind, Ast.NameWithPos(name, namePos), typeParams, typeExpr, _) => {
      TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).flatMap { case (paramTypes, withParamsEnv) =>
        TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
          TypeCons.TypeDefCon(typeDefKind, name, paramTypes, typ, namePos)
        }
      }.flatMap { typ =>
        currentEnv.addOrShadowTypeCon((name, typ), parentEnv, namePos).
          map(nextEnv => BlockContentStep(Terms.TypeDef(name), None, nextEnv))
      }
    }
    // case Ast.MethodDeclSection(targetTypeName, methods, _) =>
    //   MethodSectionAnalyzer(currentEnv).walkDeclSection(targetTypeName, methods)
    //     .map { case (term, nextEnv) => (term, None, nextEnv) }
    // case Ast.MethodDefSection(targetTypeName, selfPattern, methods, _) =>
    //   MethodSectionAnalyzer(currentEnv).walkDefSection(targetTypeName, selfPattern, methods)
    //     .map { case (term, nextEnv) => (term, None, nextEnv) }
  }

  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Terms.Block] =
    An.step(bcs.zipWithIndex)(WalkBlockState.begin) { case (currentState, (bc, index)) =>
      val WalkBlockState(currentContents, _, currentEnv) = currentState

      walkBlockContent(currentEnv, bc, index === bcs.length - 1).
        map { case BlockContentStep(contentTerm, nextReturnType, nextEnv) =>
          WalkBlockState(currentContents :+ contentTerm, nextReturnType, nextEnv)
        }
    }.flatMap { case WalkBlockState(contents, returnType, _) =>
      returnType match {
        case Some(typ) => An.result(Terms.Block(contents, typ))
        case None => An.result(Terms.Block(contents, Types.Uni)).
          tell(SourceMessage(pos, "Block should end with an expression."))
      }
    }

  def walkValueDef(currentEnv: Env, target: Ast.Pattern, valueExprTerm: Terms.ValueExpr): An[(Terms.ValueDef, Env)] = {
    PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, valueExprTerm.typ).map { case (targetTerm, nextEnv) =>
      (Terms.ValueDef(targetTerm, valueExprTerm), nextEnv)
    }
  }

}

// FIXME move
case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Seq[TypeCons.ParamCon], Env)] =
    An.step(typeParams)((Vector[TypeCons.ParamCon](), parentEnv)) { case ((currentParams, currentEnv), Ast.TypeParam(name, pos)) =>
      val paramCon = TypeCons.ParamCon(name, pos)

      currentEnv.addOrShadowTypeCon((name, paramCon), parentEnv, pos).map { nextEnv =>
        (currentParams :+ paramCon, nextEnv)
      }
    }
}
