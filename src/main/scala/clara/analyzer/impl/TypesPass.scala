package clara.analyzer.impl

import clara.asg.TypeCons
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

// The second pass of a scope, see BlockAnalyzer
object TypesPass {
  def walk(env: Env, decls: Seq[ScopeDecl]): An[Env] = An.stepDeferring(decls)(env) {
    case (env, decl: ScopeDecl.TypeDef) => typeDef(env, decl)
    case (env, ScopeDecl.Namespace(_, body)) => namespaceBody(env, body)
    case (env, _: ScopeDecl.ExtendWithoutType) => An.valid(env)
    case (env, decl: ScopeDecl.ValueDecl) => valueDecl(env, decl)
    case (env, decl: ScopeDecl.ValueDef) => valueDef(env, decl)
  }

  def typeDef(env: Env, decl: ScopeDecl.TypeDef): An[Env] =
    if (!env.ownsType(decl.name.name, decl.defId)) An.valid(env)
    else An.deferErrors(typeCon(env, decl.ast))(env).flatMap { typedEnv =>
      decl.companion.fold(An.valid(typedEnv))(namespaceBody(typedEnv, _))
    }

  def namespaceBody(env: Env, body: NamespaceBody): An[Env] =
    walk(env.enterNamespace(body.name.name, body.nsId), body.decls).map(env.leaveNamespace)

  def valueDecl(env: Env, decl: ScopeDecl.ValueDecl): An[Env] = {
    val Ast.NameWithPos(name, pos) = decl.ast.name

    TypeExprAnalyzer.typeExprType(env, decl.ast.t).flatMap { typ =>
      if (env.ownsValue(name, decl.defId)) env.defineValue(name, typ, pos) else An.valid(env)
    }
  }

  def valueDef(env: Env, decl: ScopeDecl.ValueDef): An[Env] =
    (ScopeDecl.topLevelSignature(decl.ast.target), decl.names) match {
      case (Some((_, t)), Seq(DeclaredName(Ast.NameWithPos(name, _), defId))) =>
        TypeExprAnalyzer.typeExprType(env, t).map { typ =>
          if (env.ownsValue(name, defId)) env.addValueSignature(name, typ) else env
        }
      case _ => An.valid(env)
    }

  def typeCon(env: Env, typeDef: Ast.TypeDef): An[Env] = env.localTypeCon(typeDef.target.name.name) match {
    case Some(con: TypeCons.WrapperTypeCon) => wrapperTypeCon(env, typeDef, con)
    case Some(_: TypeCons.OpaqueTypeCon | _: TypeCons.SingletonTypeCon) => An.valid(env)
    case Some(con: TypeCons.ParamCon) =>
      throw new java.lang.AssertionError(safe"type def `${con.name}` registered a type param")
    // the type def failed in the names pass
    case None => An.valid(env)
  }

  def wrapperTypeCon(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Env] =
    registerStructure(env, typeDef, con).flatMap { _ =>
      if (!ScopeDecl.constructibleKind(typeDef)) An.valid(env)
      else rejectOwnApply(env, con).flatMap(_ => TypeInterpreter.wrapperConstructorFunc(con)).map { constructorType =>
        env.addConstructor(con.name, con, constructorType, typeDef.target.name.pos)
      }
    }

  // what a wrapper type wraps, analyzed with its type params in scope
  def registerStructure(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Unit] = {
    // the names pass (NamesPass.typeCon) made sure there's a structure
    val structure = typeDef.t.get

    TypeParamAnalyzer(env).withTypeParams(con.typeParams).flatMap(
      TypeExprAnalyzer.typeExprType(_, structure)
    ).flatMap { typ =>
      An.valid(()).tellStructure(con.tcId, typ)
    }
  }

  // the companion's `apply` is the type's constructor, a member of the same name is an error until `::new`
  def rejectOwnApply(env: Env, con: TypeCons.WrapperTypeCon): An[Unit] =
    env.localCompanionNamespace(con.name).flatMap(env.namespaceMembers(_).values.get(TypeCons.ConstructorName)) match {
      case None => An.valid(())
      case Some(member) =>
        val pos = member match {
          case EnvValue(_, _, _, definedAt) => definedAt
          case EnvDeclaredValue(_, _, _, definedAt) => definedAt
          case _: EnvNamespaceRef => con.definedAt
        }

        An.error(SourceMessage(pos, safe"`${TypeCons.ConstructorName}` of `${con.name}` is its constructor, it can't be redefined yet"))
    }
}
