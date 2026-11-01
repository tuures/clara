package clara.analyzer.impl

import clara.asg.{DefId, Terms, TypeCons, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

// The first pass of a scope, see BlockAnalyzer
object NamesPass {
  def walk(env: Env, decls: Seq[ScopeDecl]): An[(Env, Vector[Terms.TypeDef])] =
    An.stepDeferring(decls)((env, Vector.empty[Terms.TypeDef])) { case ((env, terms), scopeDecl) =>
      decl(env, scopeDecl).map { case (nextEnv, declTerms) => (nextEnv, terms ++ declTerms) }
    }

  def decl(env: Env, scopeDecl: ScopeDecl): An[(Env, Seq[Terms.TypeDef])] = scopeDecl match {
    case scopeDecl: ScopeDecl.TypeDef => typeDef(env, scopeDecl)
    case ScopeDecl.Namespace(defId, body) =>
      An.deferErrors(env.addStandaloneNamespace(body.name.name, body.nsId, defId, body.name.pos))(env).
        flatMap(namespaceBody(_, body))
    case ScopeDecl.ExtendWithoutType(Ast.NameWithPos(name, pos)) =>
      An.error(SourceMessage(pos, safe"Cannot extend `$name`, there's no type `$name` defined in this scope"))
    case ScopeDecl.ValueDecl(ast, defId) => env.declareValue(ast.name.name, ast.name.pos, defId).map((_, Nil))
    case ScopeDecl.ValueDef(_, names) =>
      An.stepDeferring(names)(env)((env, name) =>
        env.declareValue(name.name.name, name.name.pos, name.defId)
      ).map((_, Nil))
  }

  // declares the name, defines the type and attaches its companion, then the names of the companion's members
  // a type def whose type con fails stays declared, and its companion's body is walked nevertheless
  def typeDef(env: Env, decl: ScopeDecl.TypeDef): An[(Env, Seq[Terms.TypeDef])] =
    env.declareType(decl.name.name, decl.defId, decl.name.pos).flatMap { declaredEnv =>
      An.deferErrors(defineType(declaredEnv, decl))((declaredEnv, Nil))
    }.flatMap { case (definedEnv, typeDefTerms) =>
      decl.companion.fold(An.valid((definedEnv, typeDefTerms))) { body =>
        namespaceBody(definedEnv, body).map { case (nextEnv, memberTerms) => (nextEnv, typeDefTerms ++ memberTerms) }
      }
    }

  def namespaceBody(env: Env, body: NamespaceBody): An[(Env, Seq[Terms.TypeDef])] =
    walk(env.enterNamespace(body.name.name, body.nsId), body.decls).map {
      case (bodyEnv, memberTerms) => (env.leaveNamespace(bodyEnv), memberTerms)
    }

  def defineType(env: Env, decl: ScopeDecl.TypeDef): An[(Env, Seq[Terms.TypeDef])] =
    typeCon(env, decl.ast).flatMap { con =>
      defineTypeName(env, decl.ast, con, decl.defId).flatMap { envWithType =>
        decl.companion.fold(An.valid(envWithType)) { body =>
          An.deferErrors(attachCompanion(envWithType, con, body))(envWithType)
        }
      }.map((_, Seq(Terms.TypeDef(con))))
    }

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

  def defineTypeName(env: Env, typeDef: Ast.TypeDef, con: TypeCons.TypeCon, defId: DefId): An[Env] = {
    val Ast.NameWithPos(name, namePos) = typeDef.target.name
    val definedEnv = env.defineType(name, con)

    con match {
      case con: TypeCons.SingletonTypeCon =>
        // adding can fail if the name is already taken by a value, but the type side is kept
        An.deferErrors(definedEnv.addValue(name, Types.Singleton(con), namePos, defId))(definedEnv)
      case _ => An.valid(definedEnv)
    }
  }

  def attachCompanion(env: Env, con: TypeCons.TypeCon, body: NamespaceBody): An[Env] = con match {
    case _: TypeCons.SingletonTypeCon =>
      An.error(SourceMessage(body.name.pos, safe"Extending a singleton isn't supported yet"))
    case con => env.addCompanionNamespace(con.name, body.nsId)
  }
}
