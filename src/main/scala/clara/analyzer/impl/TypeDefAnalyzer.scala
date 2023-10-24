package clara.analyzer.impl

import clara.asg.{Terms, TypeCons}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._


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

object TypeDefAnalyzer {
  def typeDefTerm(env: Env, typeDef: Ast.TypeDef): An[Terms.TypeDef] = {
    val Ast.TypeDef(typeDefKind, Ast.NameWithPos(name, namePos), typeParams, maybeTypeExpr, pos) = typeDef

    lazy val rejectTypeParams = An.errorIf(typeParams.length > 0)(
      SourceMessage(pos, safe"Cannot define type parameters for type $name")
    )

    lazy val rejectStructure = An.errorIf(maybeTypeExpr.isDefined)(
      SourceMessage(pos, safe"Cannot define structure for type $name")
    )

    lazy val structureTypeExpr = An.fromSomeOrError(
      maybeTypeExpr,
      SourceMessage(pos, safe"Structure needs to be defined for type $name")
    )

    (typeDefKind match {
      case wrapperTypeDefKind: Ast.TypeDefKind.Wrapper =>
        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(structureTypeExpr).
          flatMap { case ((withParamsEnv, paramTypes), typeExpr) =>
            TypeExprAnalyzer.typeExprType(withParamsEnv, typeExpr).map { typ =>
              TypeCons.WrapperTypeCon(wrapperTypeDefKind, name, paramTypes, typ, namePos)
            }
          }
      case Ast.TypeDefKind.Opaque =>
        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(rejectStructure).map { case ((_, paramTypes), _) =>
          TypeCons.OpaqueTypeCon(name, paramTypes, namePos)
        }
      case Ast.TypeDefKind.Singleton =>
        rejectTypeParams.zip(rejectStructure).map { case _ =>
          TypeCons.SingletonTypeCon(name, namePos)
        }
    }).map(Terms.TypeDef(_))
  }
}

