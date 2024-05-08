package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, TypeCons, Uniq, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


case class MethodSectionAnalyzerImpl(parentEnv: Env) {

  case class DeclSectionState(env: Env, ns: Namespace[Terms.MethodDecl]) {
    def addMethodDecl(con: TypeCons.TypeCon, methodDecl: Ast.MethodDecl): An[DeclSectionState] = {
      val Ast.MethodDecl(attributes, name, typeExpr, pos) = methodDecl

      val memberAttributesAn = walkMethodDeclAttributes(attributes)
      val typeAn = TypeExprAnalyzer.typeExprType(env, typeExpr)

      memberAttributesAn.zip(typeAn).flatMap { case (memberAttributes, typ) =>
        // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

        An.fromSomeOrError(ns.add((name, Terms.MethodDecl(memberAttributes, typ))), duplicateName).
          flatMap { updatedNs =>
            env.addMethod(con, (name, EnvMethod(memberAttributes, typ)), pos).map { nextEnv =>
              DeclSectionState(nextEnv, updatedNs)
            }
          }
      }
    }
  }
  object DeclSectionState {
    def begin(env: Env) = DeclSectionState(env, Namespace.empty[Terms.MethodDecl])
  }
  case class WalkDefState(env: Env, ns: Namespace[Terms.MethodDef]) {
    def addMethodDef(con: TypeCons.TypeCon, methodDef: Ast.MethodDef): An[WalkDefState] = {
      val Ast.MethodDef(attributes, name, typeOpt, body, pos) = methodDef

      val memberAttributesAn = walkMethodDefAttributes(attributes)

      val bodyTermAn = ValueExprAnalyzer.valueExprTerm(env, body).flatMap { bodyTerm =>
        typeOpt.fold(An.result(())){ targetTypeExpr =>
          TypeExprAnalyzer.typeExprType(env, targetTypeExpr).flatMap { targetType =>
            TypeInterpreter.expectAssignable(bodyTerm.typ, targetType, targetTypeExpr.pos)
          }
        }.map { case () =>
          bodyTerm
        }
      }

      memberAttributesAn.zip(bodyTermAn).flatMap { case (memberAttributes, bodyTerm) =>
        // TODO: do we really need a Namespace in the term/asg, (vs list), since we check dups at env.addMethod
        lazy val duplicateName = SourceMessage(pos, safe"Duplicate method name `$name`")

        An.fromSomeOrError(ns.add((name, Terms.MethodDef(memberAttributes, bodyTerm))), duplicateName)
          .flatMap { updatedNs =>
            env.addMethod(con, (name, EnvMethod(memberAttributes, bodyTerm.typ)), pos).map { nextEnv =>
              WalkDefState(nextEnv, updatedNs)
            }
          }
      }
    }
  }
  object WalkDefState {
    def begin(env: Env) = WalkDefState(env, Namespace.empty[Terms.MethodDef])
  }

  def targetCon(targetTypeName: Ast.NameWithPos): An[TypeCons.TypeCon] =
    TypeExprAnalyzer.namedTypeCon(parentEnv, targetTypeName)

  def methodSectionTerm(methodSection: Ast.MethodSection): An[(Env, Terms.MethodSection)] = {
    val Ast.MethodSection(isDecl, targetTypeName, selfPattern, methodAsts, _) = methodSection

    targetCon(targetTypeName).flatMap { con =>
      (isDecl, selfPattern) match {
        case (true, None) => methodDeclSection(con, methodAsts)
        case (true, Some(p)) => An.error(SourceMessage(p.pos, "Method declaration section cannot define self pattern"))
        case (false, Some(p)) => methodDefSection(con, p, methodAsts)
        case (false, None) => An.error(SourceMessage(targetTypeName.pos, "Self pattern expected after the type name"))
      }
    }
  }

  def methodDeclSection(targetCon: TypeCons.TypeCon, methodAsts: Seq[Ast.Method]) = {
    An.step(methodAsts)(DeclSectionState.begin(parentEnv)){ case (currentState, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDef => An.error(SourceMessage(methodAst.pos, "Method declaration expected"))
        case methodDecl: Ast.MethodDecl => currentState.addMethodDecl(targetCon, methodDecl)
      })
    }.map { case DeclSectionState(nextEnv, ns) =>
      (nextEnv, Terms.MethodDeclSection(targetCon, ns))
    }
  }

  def methodDefSection(targetCon: TypeCons.TypeCon, selfPattern: Ast.Pattern, methodAsts: Seq[Ast.Method]) = {
    val targetType = TypeCons.pseudoInstantiate(targetCon)

    PatternAnalyzer(parentEnv, parentEnv).
      walkAssignment(selfPattern, Some(targetType)).flatMap { case (selfEnv, selfPatternTerm) =>
        An.step(methodAsts)(WalkDefState.begin(selfEnv)){ case (currentState, methodAst) =>
          (methodAst match {
            case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
            case methodDef: Ast.MethodDef => currentState.addMethodDef(targetCon, methodDef)
          })
        }.map { case WalkDefState(nextEnv, ns) =>
          (nextEnv, Terms.MethodDefSection(targetCon, selfPatternTerm, ns))
        }
      }
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

object MethodSectionAnalyzer {
  def methodSectionTerm(env: Env, methodSection: Ast.MethodSection): An[(Env, Terms.MethodSection)] =
    MethodSectionAnalyzerImpl(env).methodSectionTerm(methodSection)
}
