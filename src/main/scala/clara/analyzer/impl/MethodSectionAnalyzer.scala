package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Uniq, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class MethodSectionAnalyzer(env: Env) {

  case class WalkDeclState(ns: Namespace[Terms.MethodDecl], env: Env)
  object WalkDeclState {
    def begin(env: Env) = WalkDeclState(Namespace.empty[Terms.MethodDecl], env)
  }

  def walkMethodDecl(targetType: Types.Type, uniq: Uniq, currentState: WalkDeclState, methodDecl: Ast.MethodDecl): An[WalkDeclState] = {
    val Ast.MethodDecl(attributes, name, typeExpr, pos) = methodDecl

    val memberAttributesAn = walkMethodDeclAttributes(attributes)
    val typeAn = TypeExprAnalyzer(env).walkTypeExpr(typeExpr)
    memberAttributesAn.zip(typeAn).flatMap { case (memberAttributes, typ) =>
      // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
      lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

      An.fromSomeOrError(currentState.ns.add((name, Terms.MethodDecl(memberAttributes, typ))), duplicateName)
        .flatMap { updatedNs =>
          currentState.env.addMethod(targetType, uniq, (name, EnvMethod(memberAttributes, typ)), pos).map { nextEnv =>
            WalkDeclState(updatedNs, nextEnv)
          }
        }
    }
  }

  def walkDeclSection(targetTypeName: Ast.TypeName, methodAsts: Seq[Ast.Method]) = {
    walkTargetTypeName(targetTypeName).flatMap { case (targetType, uniq) =>
      An.step(methodAsts)(WalkDeclState.begin(env)){ case (currentState, methodAst) =>
        (methodAst match {
          case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
          case methodDecl: Ast.MethodDecl => walkMethodDecl(targetType, uniq, currentState, methodDecl)
        })
      }.map { case WalkDeclState(ns, nextEnv) =>
        (Terms.MethodDeclSection(targetType, ns), nextEnv)
      }
    }
  }

  case class WalkDefState(ns: Namespace[Terms.MethodDef], env: Env)
  object WalkDefState {
    def begin(env: Env) = WalkDefState(Namespace.empty[Terms.MethodDef], env)
  }

  def walkMethodDef(targetType: Types.Type, uniq: Uniq, currentState: WalkDefState, methodDef: Ast.MethodDef): An[WalkDefState] = {
    val Ast.MethodDef(attributes, name, typeOpt, body, pos) = methodDef

    typeOpt.foreach(_ => ???) // FIXME

    val memberAttributesAn = walkMethodDefAttributes(attributes)
    val bodyTermAn = ValueExprAnalyzer(currentState.env).walkValueExpr(body)

    memberAttributesAn.zip(bodyTermAn).flatMap { case (memberAttributes, bodyTerm) =>
      // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
      lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

      An.fromSomeOrError(currentState.ns.add((name, Terms.MethodDef(memberAttributes, bodyTerm))), duplicateName)
        .flatMap { updatedNs =>
          currentState.env.addMethod(targetType, uniq, (name, EnvMethod(memberAttributes, bodyTerm.typ)), pos).map { nextEnv =>
            WalkDefState(updatedNs, nextEnv)
          }
        }
    }
  }

  def walkDefSection(targetTypeName: Ast.TypeName, selfPattern: Ast.Pattern, methodAsts: Seq[Ast.Method]) = {
    walkTargetTypeName(targetTypeName).flatMap { case (targetType, uniq) =>
      PatternAnalyzer(env, env).walkAssignment(selfPattern, targetType).map((targetType, uniq, _))
    }.flatMap { case (targetType, uniq, (selfPatternTerm, selfEnv)) =>
      An.step(methodAsts)(WalkDefState.begin(selfEnv)){ case (currentState, methodAst) =>
        (methodAst match {
          case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
          case methodDef: Ast.MethodDef => walkMethodDef(targetType, uniq, currentState, methodDef)
        })
      }.map { case WalkDefState(ns, nextEnv) =>
        (Terms.MethodDefSection(targetType, selfPatternTerm, ns), nextEnv)
      }
    }
  }

  private def walkTargetType(pos: Pos)(typ: Types.Type): An[(Types.Type, Uniq)] = typ match {
    // case Types.Alias(_, wrappedType) => walkTargetType(pos)(wrappedType)
    // case u: Types.Unique => An.result(u)
    // TODO: ForAll
    // TODO allow methods also for other kinds of types
    // (record should not have method if field with same name exists)
    case t => An.error(SourceMessage(pos, safe"Cannot have methods for type `${Types.toSource(t)}`"))
  }

  def walkTargetTypeName(targetType: Ast.TypeName): An[(Types.Type, Uniq)] = {
    val Ast.TypeName(name, pos) = targetType
    env.useType(name, pos).flatMap(walkTargetType(pos))
  }

  def walkMethodDeclAttributes(attributes: Seq[Ast.Attribute]): An[Attributes.MethodAttributes] = {
    // TODO error for duplicate attributes?
    // TODO first reduce the seq into a Namespace, then build MethodAttributes from that?
    An.step(attributes)(Attributes.MethodAttributes()) { case (attributes, Ast.Attribute(key, value, pos)) =>
      (key, value) match {
        case ("emitKind", Some("binaryOperator")) =>
          An.result(attributes.copy(emitKind = Some(Attributes.BinaryOperator)))
        case ("emitKind", Some("instanceProperty")) =>
          An.result(attributes.copy(emitKind = Some(Attributes.InstanceProperty)))
        case ("emitName", Some(emitName)) =>
          An.result(attributes.copy(emitName = Some(emitName)))
        case _ => An.error(SourceMessage(pos, "Invalid attribute for a method declaration"))
      }
    }
  }

  def walkMethodDefAttributes(attributes: Seq[Ast.Attribute]): An[Attributes.MethodAttributes] = {
    // TODO error for duplicate attributes?
    An.step(attributes)(Attributes.MethodAttributes()) { case (_, Ast.Attribute(_, _, pos)) =>
      An.error(SourceMessage(pos, "Invalid attribute for a method definition"))
    }
  }

}
