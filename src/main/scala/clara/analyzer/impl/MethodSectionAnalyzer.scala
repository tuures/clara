package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class MethodSectionAnalyzer(env: Env) {

  case class WalkDeclState(ns: Namespace[Terms.MethodDecl], env: Env)
  object WalkDeclState {
    def begin(env: Env) = WalkDeclState(Namespace.empty[Terms.MethodDecl], env)
  }

  def walkDeclSection(targetType: Ast.TypeName, methodAsts: Seq[Ast.Method]) = {
    walkTargeTypeName(targetType).flatMap { targetType =>
      An.step(methodAsts)(WalkDeclState.begin(env)){ case (state, methodAst) =>
        (methodAst match {
          case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
          case Ast.MethodDecl(attributes, name, typeExpr, pos) =>
            val memberAttributesAn = walkMethodDeclAttributes(attributes)
            val typeAn = TypeExprAnalyzer(env).walkTypeExpr(typeExpr)
            memberAttributesAn.zip(typeAn).flatMap { case (memberAttributes, typ) =>
              // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
              lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

              An.someOrError(state.ns.add((name, Terms.MethodDecl(memberAttributes, typ))), duplicateName)
                .flatMap { updatedNs =>
                  state.env.addMethod(targetType, (name, EnvMethod(memberAttributes, typ)), pos).map { nextEnv =>
                    WalkDeclState(updatedNs, nextEnv)
                  }
                }
            }
        })
      }.map { case WalkDeclState(ns, nextEnv) =>
        (Terms.MethodDeclSection(targetType, ns), None /*FIXME remove usless*/, nextEnv)
      }
    }
  }

  case class WalkDefState(ns: Namespace[Terms.MethodDef], env: Env)
  object WalkDefState {
    def begin(env: Env) = WalkDefState(Namespace.empty[Terms.MethodDef], env)
  }

  def walkDefSection(targetPattern: Ast.Pattern, methodAsts: Seq[Ast.Method]) = {
    (targetPattern match {
      case Ast.PatternAs(p, t, _) => An.result((p, t))
      case p => An.error(SourceMessage(p.pos, "Expected type assertion on top level"))
    }).flatMap { case (selfPattern, targetTypeExpr) =>
      walkTargetTypeExpr(targetTypeExpr).flatMap { targetType =>
        PatternAnalyzer(env, env).walkAssignment(selfPattern, targetType).map((_, targetType))
      }.flatMap { case ((selfPatternTerm, selfEnv), targetType) =>
        An.step(methodAsts)(WalkDefState.begin(env)){ case (state, methodAst) =>
          (methodAst match {
            case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
            case Ast.MethodDef(attributes, name, typeOpt, body, pos) =>
              typeOpt.foreach(_ => ???) // FIXME

              val memberAttributesAn = walkMethodDefAttributes(attributes)
              val bodyTermAn = ValueExprAnalyzer(selfEnv).walkValueExpr(body)

              memberAttributesAn.zip(bodyTermAn).flatMap { case (memberAttributes, bodyTerm) =>
                // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
                lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

                An.someOrError(state.ns.add((name, Terms.MethodDef(memberAttributes, bodyTerm))), duplicateName)
                  .flatMap { updatedNs =>
                    state.env.addMethod(targetType, (name, EnvMethod(memberAttributes, bodyTerm.typ)), pos).map { nextEnv =>
                      WalkDefState(updatedNs, nextEnv)
                    }
                  }
              }
          })
        }.map { case WalkDefState(ns, nextEnv) =>
          (Terms.MethodDefSection(targetType, selfPatternTerm, ns), None /*FIXME remove usless*/, nextEnv)
        }
      }
    }
  }

  private def walkTargetType(pos: Pos)(typ: Types.Type): An[Types.Unique] = typ match {
    case Types.Alias(_, wrappedType) => walkTargetType(pos)(wrappedType)
    case u: Types.Unique => An.result(u)
    // TODO: ForAll
    // TODO allow methods also for other kinds of types
    // (record should not have method if field with same name exists)
    case t => An.error(SourceMessage(pos, safe"Cannot have methods for type `${Types.toSource(t)}`"))
  }

  def walkTargeTypeName(targetType: Ast.TypeName): An[Types.Unique] = {
    env.useType(targetType.name, targetType.pos).flatMap(walkTargetType(targetType.pos))
  }

  def walkTargetTypeExpr(targetTypeExpr: Ast.TypeExpr): An[Types.Unique] = {
    TypeExprAnalyzer(env).walkTypeExpr(targetTypeExpr).flatMap(walkTargetType(targetTypeExpr.pos))
  }

  def walkMethodDeclAttributes(attributes: Seq[Ast.Attribute]): An[Attributes.MethodAttributes] = {
    // TODO error for duplicate attributes?
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
