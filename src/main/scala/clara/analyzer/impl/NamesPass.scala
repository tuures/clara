package clara.analyzer.impl

import clara.asg.{Terms, TypeCons, Types}
import clara.ast.{Ast, SourceMessage}

import clara.util.Safe._

// The first pass of a block, see BlockAnalyzer
object NamesPass {
  def walk(env: Env, block: IndexedBlock): An[(Env, Vector[Terms.TypeDef])] =
    An.stepDeferring(block.contents)((env, Vector.empty[Terms.TypeDef])) { case ((env, terms), blockContent) =>
      content(env, block, blockContent).map { case (nextEnv, contentTerms) => (nextEnv, terms ++ contentTerms) }
    }

  def content(env: Env, block: IndexedBlock, blockContent: IndexedBlock.Content): An[(Env, Seq[Terms.TypeDef])] = blockContent match {
    case td: IndexedBlock.TypeDef => typeDef(env, block, td)
    case IndexedBlock.Namespace(Ast.NamespaceDef(_, Ast.NameWithPos(name, pos), _, _), defId, body) =>
      An.deferErrors(env.addStandaloneNamespace(name, body.nsId, defId, pos))(env).flatMap(namespaceBody(_, body))
    case IndexedBlock.Extend(Ast.NamespaceDef(_, Ast.NameWithPos(name, pos), _, _), _) =>
      (block.firstTypeDef(name).map(_.ast.typeDefKind) match {
        case None => An.error(SourceMessage(pos, safe"Cannot extend `$name`, there's no type `$name` defined in this scope"))
        case Some(Ast.TypeDefKind.Singleton) => An.error(SourceMessage(pos, safe"Extending a singleton isn't supported yet"))
        case Some(_) => An.valid(())
      }).map(_ => (env, Nil))
    case _: IndexedBlock.MethodSection => An.valid((env, Nil))
    case IndexedBlock.ValueDecl(ast, defId) => env.declareValue(ast.name.name, ast.name.pos, defId).map((_, Nil))
    case IndexedBlock.SingleTypedValueDef(_, name, _) => declareValues(env, Seq(name)).map((_, Nil))
    case IndexedBlock.ValueDef(_, names) => declareValues(env, names).map((_, Nil))
    case _: IndexedBlock.ValueExpr => An.valid((env, Nil))
  }

  def declareValues(env: Env, names: Seq[IndexedBlock.DeclaredName]): An[Env] =
    An.stepDeferring(names)(env) { case (env, IndexedBlock.DeclaredName(Ast.NameWithPos(name, pos), defId)) =>
      env.declareValue(name, pos, defId)
    }

  def typeDef(env: Env, block: IndexedBlock, typeDef: IndexedBlock.TypeDef): An[(Env, Seq[Terms.TypeDef])] =
    typeCon(env, typeDef.ast).flatMap { con =>
      An.deferErrors(defineType(env, block, typeDef, con))((env, Nil))
    }.flatMap { case (definedEnv, typeDefTerms) =>
      block.companion(typeDef).fold(An.valid((definedEnv, typeDefTerms))) { body =>
        namespaceBody(definedEnv, body).map { case (nextEnv, memberTerms) => (nextEnv, typeDefTerms ++ memberTerms) }
      }
    }

  def namespaceBody(env: Env, body: IndexedBlock.NamespaceBody): An[(Env, Seq[Terms.TypeDef])] =
    walk(env.enterNamespace(body.name, body.nsId), body.block).map {
      case (bodyEnv, memberTerms) => (env.leaveNamespace(bodyEnv), memberTerms)
    }

  // the type, the singleton's value and the companion
  def defineType(
    env: Env,
    block: IndexedBlock,
    typeDef: IndexedBlock.TypeDef,
    con: TypeCons.TypeCon,
  ): An[(Env, Seq[Terms.TypeDef])] = {
    val Ast.NameWithPos(name, pos) = typeDef.name

    env.addTypeCon(name, con, pos, typeDef.defId).flatMap { envWithType =>
      val envWithValue = con match {
        case con: TypeCons.SingletonTypeCon =>
          An.deferErrors(envWithType.addValue(name, Types.Singleton(con), pos, typeDef.defId))(envWithType)
        case _ => An.valid(envWithType)
      }

      envWithValue.flatMap { envWithValue =>
        (con, block.companion(typeDef)) match {
          // its `::extend`s are an error
          case (_: TypeCons.SingletonTypeCon, _) | (_, None) => An.valid(envWithValue)
          case (con, Some(body)) => envWithValue.addCompanionNamespace(con.name, body.nsId)
        }
      }
    }.map((_, Seq(Terms.TypeDef(con))))
  }

  // the type con of a type def without analyzing what it wraps
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

    val kindErrors = typeDefKind match {
      case _: Ast.TypeDefKind.Wrapper => requireStructure
      case Ast.TypeDefKind.Opaque => rejectStructure
      case Ast.TypeDefKind.Singleton => rejectTypeParams.zip(rejectStructure).map(_ => ())
    }

    An.deferErrors(kindErrors)(()).flatMap(_ => TypeParamAnalyzer(env).walkTypeParams(typeParams)).map {
      case (_, typeParamCons) => typeDefKind match {
        case wrapperTypeDefKind: Ast.TypeDefKind.Wrapper =>
          TypeCons.WrapperTypeCon(wrapperTypeDefKind, name, typeParamCons, env.nsPath, namePos)
        case Ast.TypeDefKind.Opaque => TypeCons.OpaqueTypeCon(name, typeParamCons, env.nsPath, namePos)
        case Ast.TypeDefKind.Singleton => TypeCons.SingletonTypeCon(name, env.nsPath, namePos)
      }
    }
  }
}
