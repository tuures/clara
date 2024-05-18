package clara.analyzer.impl

import clara.asg.{Terms, Types, TypeCons}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

object TypeDefAnalyzer {
  def typeDefTerm(env: Env, allowShadow: Env, typeDef: Ast.TypeDef): An[(Env, Terms.TypeDef)] = {
    val Ast.TypeDef(typeDefKind, target, maybeTypeExpr, pos) = typeDef
    val Ast.DeclTargetType(Ast.NameWithPos(name, namePos), typeParams, targetPos) = target

    lazy val rejectTypeParams = An.errorIf(typeParams.length > 0)(
      SourceMessage(targetPos, safe"Cannot define type parameters for type `$name`")
    )

    lazy val rejectStructure = An.errorFromSome(maybeTypeExpr)(maybeTypeExpr =>
      SourceMessage(maybeTypeExpr.pos, safe"Cannot define structure for type `$name`")
    )

    lazy val structureTypeExpr = An.fromSomeOrError(
      maybeTypeExpr,
      SourceMessage(pos, safe"Structure needs to be defined for type `$name`")
    )

    val typeConAn: An[TypeCons.TypeCon] = typeDefKind match {
      case wrapperTypeDefKind: Ast.TypeDefKind.Wrapper =>
        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(structureTypeExpr).
          flatMap { case ((withTypeParamsEnv, typeParamCons), typeExpr) =>
            TypeExprAnalyzer.typeExprType(withTypeParamsEnv, typeExpr).map { typ =>
              TypeCons.WrapperTypeCon(wrapperTypeDefKind, name, typeParamCons, typ, namePos)
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
    }

    // TODO extract to separate method?
    typeConAn.flatMap { typeCon =>
      (typeCon match {
        case con: TypeCons.SingletonTypeCon =>
          val typ = Types.Singleton(con)

          env.addOrShadowTypeCon((name, con), allowShadow, namePos).
            flatMap(_.addOrShadowValue((name, typ), allowShadow, namePos))

        case con =>
          // FIXME add constructor function to value env
          env.addOrShadowTypeCon((name, con), allowShadow, namePos)

      }).map(nextEnv => (nextEnv, Terms.TypeDef(typeCon)))
    }
  }
}

