package clara.analyzer.impl

import clara.asg.{DefId, Types, TypeCons}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

object TypeDefAnalyzer {
  // the type con of a type def with its type params, without analyzing what it wraps
  def typeCon(env: Env, typeDef: Ast.TypeDef): An[TypeCons.TypeCon] = {
    val Ast.TypeDef(typeDefKind, target, maybeTypeExpr, pos) = typeDef
    val Ast.DeclTargetType(Ast.NameWithPos(name, namePos), typeParams, targetPos) = target

    lazy val rejectTypeParams = An.errorIf(typeParams.length > 0)(
      SourceMessage(targetPos, safe"Cannot define type parameters for type `$name`")
    )

    lazy val rejectStructure = An.errorFromSome(maybeTypeExpr)(maybeTypeExpr =>
      SourceMessage(maybeTypeExpr.pos, safe"Cannot define structure for type `$name`")
    )

    lazy val requireStructure = An.errorIf(maybeTypeExpr.isEmpty)(
      SourceMessage(pos, safe"Structure needs to be defined for type `$name`")
    )

    typeDefKind match {
      case wrapperTypeDefKind: Ast.TypeDefKind.Wrapper =>
        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(requireStructure).map { case ((_, typeParamCons), _) =>
          TypeCons.WrapperTypeCon(wrapperTypeDefKind, name, typeParamCons, env.nsPath, namePos)
        }
      case Ast.TypeDefKind.Opaque =>
        TypeParamAnalyzer(env).walkTypeParams(typeParams).zip(rejectStructure).map { case ((_, typeParamCons), _) =>
          TypeCons.OpaqueTypeCon(name, typeParamCons, env.nsPath, namePos)
        }
      case Ast.TypeDefKind.Singleton =>
        rejectTypeParams.zip(rejectStructure).map { case _ =>
          TypeCons.SingletonTypeCon(name, env.nsPath, namePos)
        }
    }
  }

  // TODO: can be combined with the above? together they are the names pass for a single type def
  def registerName(env: Env, typeDef: Ast.TypeDef, con: TypeCons.TypeCon): An[Env] = {
    val Ast.NameWithPos(name, namePos) = typeDef.target.name

    con match {
      case con: TypeCons.SingletonTypeCon =>
        // the type and the value are the same declaration, using either one uses it
        val defId = DefId()

        env.addTypeCon(name, con, namePos, defId).
          flatMap(_.addValue(name, Types.Singleton(con), namePos, defId))
      case con => env.addTypeCon(name, con, namePos)
    }
  }

  // what a registered wrapper type wraps, analyzed with its type params in scope (the types pass)
  def registerStructure(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Unit] = {
    val withTypeParamsEnv = con.typeParams.foldLeft(An.valid(env.startNestedScope)) { (envAn, paramCon) =>
      envAn.flatMap(_.addTypeCon(paramCon.name, paramCon, paramCon.definedAt))
    }

    // then names pass (def typeCon) made sure there's a structure
    val structure = typeDef.t.get
    withTypeParamsEnv.flatMap(TypeExprAnalyzer.typeExprType(_, structure)).flatMap { typ =>
      An.valid(()).tellStructure(con.uniq, typ)
    }
  }
}
