package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._


case class MethodSectionAnalyzer(env: Env) {

  def walkDeclSection(targetTypeExpr: Ast.TypeExpr, methodAsts: Seq[Ast.Method], pos: Pos) = {
    walkTargetType(targetTypeExpr).flatMap { targetType =>
      walkMethodDecls(env, methodAsts).map { methodDeclNs =>
        Terms.MethodDeclSection(targetType, methodDeclNs)
      }.flatMap { methodSection =>
        env.addMethods((targetType, methodSection), pos).map { nextEnv =>
          (methodSection, None, nextEnv)
        }
      }
    }
  }

  def walkDefSection(targetPattern: Ast.Pattern, methodAsts: Seq[Ast.Method], pos: Pos) = {
    (targetPattern match {
      case Ast.PatternAs(p, t, _) => An.result((p, t))
      case p => An.error(SourceMessage(p.pos, "Expected type assertion on top level"))
    }).flatMap { case (selfPattern, targetTypeExpr) =>
      walkTargetType(targetTypeExpr).flatMap { targetType =>
        PatternAnalyzer(env, env).walkPattern(selfPattern, targetType).map((_, targetType))
      }.flatMap { case ((selfPatternTerm, selfEnv), targetType) =>
        walkMethodDefs(selfEnv, methodAsts).map { methodDefNs =>
          Terms.MethodDefSection(targetType, selfPatternTerm, methodDefNs)
        }.flatMap { methodSection =>
          env.addMethods((targetType, methodSection), pos).map { nextEnv =>
            (methodSection, None, nextEnv)
          }
        }
      }
    }
  }

  def walkTargetType(targetTypeExpr: Ast.TypeExpr) =
    TypeExprAnalyzer(env).walkTypeExpr(targetTypeExpr).flatMap {
      case u: Types.Unique => An.result(u)
      // TODO allow methods also for other kinds of types
      case t => An.error(SourceMessage(targetTypeExpr.pos, safe"Cannot have methods for type `${Types.toSource(t)}`"))
    }

  def walkMethodDefs(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Terms.MethodDef]] = {
    An.step(methodAsts)(Namespace.empty[Terms.MethodDef]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
        case Ast.MethodDef(attributes, name, typeOpt, body, pos) =>
          typeOpt.foreach(_ => ???)

          // TODO not all the decl attributes make sense for defs?
          val memberAttributesAn = walkMemberAttributes(attributes)
          val bodyTermAn = ValueExprAnalyzer(currentEnv).walkValueExpr(body)

          memberAttributesAn.zip(bodyTermAn).flatMap { case (memberAttributes, bodyTerm) =>
            lazy val error = SourceMessage(pos, "Already defined")

            An.someOrError(ns.add((name, Terms.MethodDef(memberAttributes, bodyTerm))), error)
          }
      })
    }
  }

  def walkMethodDecls(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Terms.MethodDecl]] = {
    An.step(methodAsts)(Namespace.empty[Terms.MethodDecl]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
        case Ast.MethodDecl(attributes, name, typeExpr, pos) =>
          val memberAttributesAn = walkMemberAttributes(attributes)
          val typeAn = TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr)
          memberAttributesAn.zip(typeAn).flatMap { case (memberAttributes, typ) =>
            lazy val error = SourceMessage(pos, "Already declared")

            An.someOrError(ns.add((name, Terms.MethodDecl(memberAttributes, typ))), error)
          }
      })
    }
  }

  def walkMemberAttributes(attributes: Seq[Ast.Attribute]): An[Attributes.MemberAttributes] = {
    // TODO error for duplicate attributes?
    An.step(attributes)(Attributes.MemberAttributes()) { case (attributes, Ast.Attribute(key, value, pos)) =>
      (key, value) match {
        case ("emitKind", Some("binaryOperator")) =>
          An.result(attributes.copy(emitKind = Some(Attributes.BinaryOperator)))
        case ("emitKind", Some("instanceProperty")) =>
          An.result(attributes.copy(emitKind = Some(Attributes.InstanceProperty)))
        case ("emitName", Some(emitName)) =>
          An.result(attributes.copy(emitName = Some(emitName)))
        case _ => An.error(SourceMessage(pos, "Invalid member attribute"))
      }
    }
  }

}
