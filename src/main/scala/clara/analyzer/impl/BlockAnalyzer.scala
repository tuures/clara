package clara.analyzer.impl

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import ai.x.safe._


case class BlockAnalyzer(parentEnv: Env) {

  def walkBlock(bcs: Seq[Ast.BlockContent], pos: Pos): An[Terms.Block] =
    An.step(bcs.zipWithIndex)(WalkBlockState.begin) { case (currentState, (bc, index)) =>
      walkBlockContent(currentState, bc, index == bcs.length - 1)
    }.flatMap { case WalkBlockState(contents, returnType, _) =>
      returnType match {
        case Some(typ) => An.result(Terms.Block(contents, typ))
        case None => An.error(SourceMessage(pos, "Block must end with an expression")) // TODO: make it a warning and return
      }
    }

  case class WalkBlockState(
    currentContents: Vector[Terms.BlockContent],
    currentReturnType: Option[Types.Typ],
    currentEnv: Env
  )
  object WalkBlockState {
    def begin = WalkBlockState(Nil.toVector, None, parentEnv)
  }

  def walkBlockContent(currentState: WalkBlockState, bc: Ast.BlockContent, isLast: Boolean): An[WalkBlockState] = {
    val WalkBlockState(currentContents: Vector[Terms.BlockContent], currentReturnType: Option[Types.Typ], currentEnv: Env) = currentState

    (bc match {
      case valueExprAst: Ast.ValueExpr => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(valueExprAst).flatMap { valueExprTerm =>
          val isUnit = valueExprTerm.typ === Types.Uni

          lazy val discardWarning = SourceMessage(bc.pos, "Non-Unit value discarded in block")
          val maybeDiscardWarning = if (!isLast && !isUnit) Seq(discardWarning) else Nil

          An.result((valueExprTerm, Some(valueExprTerm.typ), currentEnv)).tell(maybeDiscardWarning)
        }
      }
      case Ast.ValueNamesDef(target, e, _) => {
        ValueExprAnalyzer(currentEnv).walkValueExpr(e).flatMap { valueExprTerm =>
          walkValueNamesDef(currentEnv, target, valueExprTerm)
        }.map { case (namesDef, nextEnv) => (namesDef, None, nextEnv) }
      }
      case Ast.TypeDef(name, typeExpr, pos) => {
        TypeExprAnalyzer(currentEnv).walkTypeExpr(typeExpr).flatMap {
          case st: Types.StructuralTyp =>
            currentEnv.addOrShadowType((name, Types.Unique(st)), parentEnv, pos).
              map(nextEnv => (Terms.TypeDef(name), None, nextEnv))
          case _ => An.error(SourceMessage(typeExpr.pos, "Structural type expected"))
        }
      }
      case Ast.MethodSection(isDeclSection, targetTypeName, methodAsts, pos) => {
        // TODO split into MethodSectionAnalyzer
        currentEnv.useType(targetTypeName, pos /* FIXME: give more accurate Pos */).flatMap { targetType =>
          (if (isDeclSection) {
            walkMethodDecls(currentEnv, methodAsts).map { methodDeclNs =>
              Terms.MethodDeclSection(targetType, methodDeclNs)
            }
          } else {
            walkMethodDefs(currentEnv, methodAsts).map { methodDefNs =>
              Terms.MethodDefSection(targetTypeName, targetType, methodDefNs)
            }
          }).flatMap { methodSection =>
            currentEnv.addMethods((targetType, methodSection), pos).map { nextEnv =>
              (methodSection, None, nextEnv)
            }
          }
        }
      }
    }).map { case (content, typ, nextEnv) =>
      WalkBlockState(currentContents :+ content, typ, nextEnv)
    }
  }

  def walkValueNamesDef(currentEnv: Env, target: Ast.Pattern, valueExprTerm: Terms.ValueExpr): An[(Terms.ValueNamesDef, Env)] = target match {
    case Ast.NamePattern(name, pos) => currentEnv.addOrShadowValue((name, valueExprTerm.typ), parentEnv, pos).map { nextEnv =>
      (Terms.ValueNamesDef(Terms.NamePattern(name), valueExprTerm), nextEnv)
    }
    case _ => ???
  }

  def walkMethodDefs(currentEnv: Env, methodAsts: Seq[Ast.Method]): An[Namespace[Terms.MethodDef]] = {
    An.step(methodAsts)(Namespace.empty[Terms.MethodDef]){ case (ns, methodAst) =>
      (methodAst match {
        case _: Ast.MethodDecl => An.error(SourceMessage(methodAst.pos, "Method definition expected"))
        case Ast.MethodDef(attributes, name, typeOpt, body, pos) =>
          typeOpt.foreach(_ => ???)

          val memberAttributesAn = walkMemberAttributes(attributes)
          val bodyTermAn = ValueExprAnalyzer(currentEnv).walkValueExpr(body)

          memberAttributesAn.zip(bodyTermAn).flatMap { case (memberAttributes, bodyTerm) =>
            lazy val error = SourceMessage(pos, "Already defined")

            // FIXME check attributes, convert to map
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

            // FIXME check attributes, convert to map
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
