package clara.analyzer.impl

import clara.asg.{TypeCons, Types}
import clara.ast.Ast

// The second pass of a block, see BlockAnalyzer
object TypesPass {
  def walk(env: Env, block: IndexedBlock): An[Env] = An.stepDeferring(block.contents)(env) {
    case (env, td: IndexedBlock.TypeDef) => typeDef(env, block, td)
    case (env, IndexedBlock.Namespace(_, _, body)) => namespaceBody(env, body)
    case (env, vd: IndexedBlock.ValueDecl) => valueDecl(env, vd)
    case (env, vd: IndexedBlock.SingleTypedValueDef) => signature(env, vd)
    case (env, _: IndexedBlock.Extend | _: IndexedBlock.MethodSection | _: IndexedBlock.ValueDef | _: IndexedBlock.ValueExpr) =>
      An.valid(env)
  }

  def typeDef(env: Env, block: IndexedBlock, typeDef: IndexedBlock.TypeDef): An[Env] = {
    val typedEnv = env.ownTypeCon(typeDef.name.name, typeDef.defId) match {
      case Some(con: TypeCons.WrapperTypeCon) => An.deferErrors(wrapperTypeCon(env, typeDef.ast, con))(env)
      case Some(_: TypeCons.OpaqueTypeCon | _: TypeCons.SingletonTypeCon | _: TypeCons.ParamCon) => An.valid(env)
      case None => duplicateStructure(env, typeDef.ast).map(_ => env)
    }

    typedEnv.flatMap(typedEnv => block.companion(typeDef).fold(An.valid(typedEnv))(namespaceBody(typedEnv, _)))
  }

  def duplicateStructure(env: Env, typeDef: Ast.TypeDef): An[Unit] = (typeDef.typeDefKind, typeDef.t) match {
    case (_: Ast.TypeDefKind.Wrapper, Some(structure)) =>
      // the names pass has reported the duplicate type params
      val params = typeDef.target.typeParams.distinctBy(_.name)
      TypeParamAnalyzer(env).walkTypeParams(params).flatMap {
        case (typeParamsEnv, _) => TypeExprAnalyzer.typeExprType(typeParamsEnv, structure)
      }.map(_ => ())
    case _ => An.valid(())
  }

  def namespaceBody(env: Env, body: IndexedBlock.NamespaceBody): An[Env] =
    walk(env.enterNamespace(body.name, body.nsId), body.block).map(env.leaveNamespace)

  def valueDecl(env: Env, valueDecl: IndexedBlock.ValueDecl): An[Env] = {
    val IndexedBlock.ValueDecl(Ast.ValueDecl(Ast.NameWithPos(name, pos), t, _), defId) = valueDecl

    TypeExprAnalyzer.typeExprType(env, t).map(env.defineValue(name, defId, _, pos))
  }

  def signature(env: Env, valueDef: IndexedBlock.SingleTypedValueDef): An[Env] =
    TypeExprAnalyzer.typeExprType(env, valueDef.signature).map(env.addSignature(valueDef.name.defId, _))

  def wrapperTypeCon(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Env] =
    registerStructure(env, typeDef, con).flatMap { _ =>
      if (!IndexedBlock.constructibleKind(typeDef) || hasOwnApply(env, con)) An.valid(env)
      else TypeInterpreter.wrapperConstructorFunc(con).map(env.addConstructor(con.name, con, _))
    }

  def registerStructure(env: Env, typeDef: Ast.TypeDef, con: TypeCons.WrapperTypeCon): An[Unit] =
    TypeParamAnalyzer(env).withTypeParams(con.typeParams).flatMap { case (typeParamsEnv, _) =>
      typeDef.t.fold[An[Types.Type]](An.valid(Types.ErrorType))(TypeExprAnalyzer.typeExprType(typeParamsEnv, _))
    }.flatMap { typ =>
      An.valid(()).tellStructure(con.tcId, typ)
    }

  def hasOwnApply(env: Env, con: TypeCons.WrapperTypeCon): Boolean =
    env.localCompanionNamespace(con.name).exists(env.namespaceMembers(_).values.get(TypeCons.ConstructorName).isDefined)
}
