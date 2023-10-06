package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._

case class BlockAnalyzerState(
  currentContents: Vector[Terms.BlockContent],
  currentReturnType: Option[Types.Type],
  currentEnv: Env
) {
  def finishTerm(blockPos: Pos): An[Terms.Block] = currentReturnType match {
    case Some(typ) => An.result(Terms.Block(currentContents, typ))
    case None => An.result(Terms.Block(currentContents, Types.Uni)).
      tell(SourceMessage(blockPos, "Block should end with an expression."))
  }
}
object BlockAnalyzerState {
  def begin(parentEnv: Env) = BlockAnalyzerState(Nil.toVector, None, parentEnv)
}

case class BlockAnalyzer(parentEnv: Env) {

  case class BlockContentStep(contentTerm: Terms.BlockContent, nextReturnType: Option[Types.Type], nextEnv: Env)

  def walkBlockContent(currentEnv: Env, bc: Ast.BlockContent, isLast: Boolean): An[BlockContentStep] = bc match {
    case valueExprAst: Ast.ValueExpr =>
      ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprTerm =>
        val isUnit = valueExprTerm.typ === Types.Uni

        lazy val discardWarning = SourceMessage(bc.pos, "Non-unit value discarded in block")
        val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

        An.result(BlockContentStep(valueExprTerm, Some(valueExprTerm.typ), currentEnv)).tell(maybeDiscardWarning)
      }
    case Ast.ValueDecl(name, t, pos) =>
      TypeExprAnalyzer(currentEnv).walkTypeExpr(t).flatMap { typ =>
        currentEnv.addOrShadowValue((name, typ), parentEnv, pos)
      }.map { nextEnv =>
        BlockContentStep(Terms.ValueDecl(name), None, nextEnv)
      }
    case Ast.ValueDef(target, e, _) =>
      ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
        PatternAnalyzer(currentEnv, parentEnv).walkAssignment(target, valueExprTerm.typ).
          map { case (targetTerm, nextEnv) =>
            BlockContentStep(Terms.ValueDef(targetTerm, valueExprTerm), None, nextEnv)
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
    case Ast.TypeDef(typeDefKind, Ast.NameWithPos(name, namePos), typeParams, maybeTypeExpr, pos) =>
      val typeConAn: An[TypeCons.TypeCon] = typeDefKind match {
        case _: Ast.TypeDefKind.Wrapper =>
          lazy val missingStructureError = An.error(SourceMessage(pos,
            safe"Structure needs to be defined for type $name")
          )
          val typeExprAn = maybeTypeExpr.map(An.result(_)).getOrElse(missingStructureError)

          TypeParamAnalyzer(currentEnv).walkTypeParams(typeParams).zip(typeExprAn).
            flatMap { case ((paramTypes, withParamsEnv), typeExpr) =>
              TypeExprAnalyzer(withParamsEnv).walkTypeExpr(typeExpr).map { typ =>
                TypeCons.TypeDefCon(typeDefKind, name, paramTypes, Some(typ), namePos)
              }
            }
        case _: Ast.TypeDefKind.Solitary =>
          lazy val structureGivenError = An.error(SourceMessage(pos, safe"Cannot define structure for type $name"))
          lazy val typeParamsGivenError = An.error(SourceMessage(pos, safe"Cannot define type parameters for type $name"))

          (if (typeParams.length > 0) typeParamsGivenError else An.result(())).flatMap { case () =>
            (if (maybeTypeExpr.isDefined) structureGivenError else An.result(()))
          }.map { case () =>
            TypeCons.TypeDefCon(typeDefKind, name, Nil, None, namePos)
          }
      }

    typeConAn.flatMap { typeCon =>
      currentEnv.addOrShadowTypeCon((name, typeCon), parentEnv, namePos).
        map(nextEnv => BlockContentStep(Terms.TypeDef(name), None, nextEnv))
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
      val BlockAnalyzerState(currentContents, _, currentEnv) = currentState

      walkBlockContent(currentEnv, bc, index === bcs.length - 1).
        map { case BlockContentStep(contentTerm, nextReturnType, nextEnv) =>
          BlockAnalyzerState(currentContents :+ contentTerm, nextReturnType, nextEnv)
        }
    }

  def walkBlock(block: Ast.Block): An[Terms.Block] = walkBlockContents(block.bcs).flatMap(_.finishTerm(block.pos))
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
