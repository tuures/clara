package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons}
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

case class BlockAnalyzer(parentEnv: Env) {

  case class BlockContentStep(nextEnv: Env, contentTerm: Terms.BlockContent, nextReturnType: Option[Types.Type])

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(currentEnv, valueExprTerm, Some(valueExprTerm.typ))).tell(maybeDiscardWarning)
      }
    case Ast.ValueDecl(name, t, pos) =>
      TypeExprAnalyzer(currentEnv).walkTypeExpr(t).flatMap { typ =>
        // FIXME use namePos to be consistent with valueDef and typeDef
        currentEnv.addOrShadowValue((name, typ), parentEnv, pos)
      }.map { nextEnv =>
        BlockContentStep(nextEnv, Terms.ValueDecl(name), None)
      }
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
        PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, valueExprTerm.typ).
          map { case (targetTerm, nextEnv) =>
            BlockContentStep(nextEnv, Terms.ValueDef(targetTerm, valueExprTerm), None)
          }
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
    case typeDef: Ast.TypeDef => TypeDefAnalyzer.walkTypeDef(currentEnv, typeDef).flatMap { typeDefTerm =>
      val Ast.NameWithPos(name, namePos) = typeDef.name

      currentEnv.addOrShadowTypeCon((name, typeDefTerm.con), parentEnv, namePos).
        map(nextEnv => BlockContentStep(nextEnv, typeDefTerm, None))
    }
    // case Ast.MethodDeclSection(targetTypeName, methods, _) =>
    //   MethodSectionAnalyzer(currentEnv).walkDeclSection(targetTypeName, methods)
    //     .map { case (term, nextEnv) => (term, None, nextEnv) }
    // case Ast.MethodDefSection(targetTypeName, selfPattern, methods, _) =>
    //   MethodSectionAnalyzer(currentEnv).walkDefSection(targetTypeName, selfPattern, methods)
    //     .map { case (term, nextEnv) => (term, None, nextEnv) }
  }

  def walkBlockContents(bcs: Seq[Ast.BlockContent]): An[BlockAnalyzerState] =
    An.step(bcs.zipWithIndex)(BlockAnalyzerState.begin(parentEnv)) { case (currentState, (bc, index)) =>
      val BlockAnalyzerState(currentEnv, currentContents, _) = currentState

      walkBlockContent(currentEnv, bc, index === bcs.length - 1).
        map { case BlockContentStep(nextEnv, contentTerm, nextReturnType) =>
          BlockAnalyzerState(nextEnv, currentContents :+ contentTerm, nextReturnType)
        }
    }

  def walkBlock(block: Ast.Block): An[Terms.Block] = walkBlockContents(block.bcs).flatMap(_.finishTerm(block.pos))
}

// FIXME move
case class TypeParamAnalyzer(parentEnv: Env) {
  def walkTypeParams(typeParams: Seq[Ast.TypeParam]): An[(Env, Seq[TypeCons.ParamCon])] =
    An.step(typeParams)((parentEnv, Vector[TypeCons.ParamCon]())) { case ((currentEnv, currentParams), Ast.TypeParam(name, pos)) =>
      val paramCon = TypeCons.ParamCon(name, pos)

      currentEnv.addOrShadowTypeCon((name, paramCon), parentEnv, pos).map { nextEnv =>
        (nextEnv, currentParams :+ paramCon)
      }
    }
}

// FIXME move
object TypeDefAnalyzer {
  def walkTypeDef(env: Env, typeDef: Ast.TypeDef): An[Terms.TypeDef] = {
    val Ast.TypeDef(typeDefKind, Ast.NameWithPos(name, namePos), typeParams, maybeTypeExpr, pos) = typeDef

    (typeDefKind match {
      case wrapperTypeDefKind: Ast.TypeDefKind.Wrapper =>
        lazy val missingStructureError = An.error(SourceMessage(pos,
          safe"Structure needs to be defined for type $name")
        )
        val typeExprAn = maybeTypeExpr.map(An.result(_)).getOrElse(missingStructureError)

        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(typeExprAn).
          flatMap { case ((withParamsEnv, paramTypes), typeExpr) =>
            TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
              TypeCons.WrapperTypeCon(wrapperTypeDefKind, name, paramTypes, typ, namePos)
            }
          }
      case solitaryTypeDefKind: Ast.TypeDefKind.Solitary =>
        val rejectTypeParams = An.errorIf(typeParams.length > 0)(
          SourceMessage(pos, safe"Cannot define type parameters for type $name")
        )

        val rejectStructure = An.errorIf(maybeTypeExpr.isDefined)(
          SourceMessage(pos, safe"Cannot define structure for type $name")
        )

        rejectTypeParams.zip(rejectStructure).map { case _ =>
          TypeCons.SolitaryTypeCon(solitaryTypeDefKind, name, namePos)
        }
    }).map(Terms.TypeDef(_))
  }
}
