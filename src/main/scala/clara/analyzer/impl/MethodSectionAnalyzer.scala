package clara.analyzer.impl

import clara.asg.{Attributes, DefId, Terms, Types, TypeCons, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}


case class MethodSectionAnalyzerImpl(env: Env, targetCon: TypeCons.TypeCon) {
  def walkMethodDeclAttributes(attributes: Seq[Ast.Attribute]): An[Attributes.MethodAttributes] = {
    // TODO error for duplicate attributes?
    // TODO first reduce the seq into a Namespace, then build MethodAttributes from that?
    An.step(attributes)(Attributes.MethodAttributes()) { case (attributes, Ast.Attribute(key, value, pos)) =>
      (key, value) match {
        case ("emitKind", Some("binaryOperator")) =>
          An.valid(attributes.copy(emitKind = Some(Attributes.BinaryOperator)))
        case ("emitKind", Some("instanceProperty")) =>
          An.valid(attributes.copy(emitKind = Some(Attributes.InstanceProperty)))
        case ("emitName", Some(emitName)) =>
          An.valid(attributes.copy(emitName = Some(emitName)))
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

  def withTargetType[A](typeParams: Seq[Ast.TypeParam], pos: Pos)(
    f: (Env, Seq[TypeCons.ParamCon], Types.Type) => An[A]
  ): An[A] =
    TypeParamAnalyzer(env).walkTypeParams(typeParams).flatMap { case (typeParamsEnv, paramCons) =>
      An.deferErrors(targetType(paramCons, pos))(Types.ErrorType).flatMap(f(typeParamsEnv, paramCons, _))
    }

  def targetType(paramCons: Seq[TypeCons.ParamCon], pos: Pos): An[Types.Type] = targetCon match {
    case TypeCons.WrapperTypeCon(Ast.TypeDefKind.Alias, _, _, _, _, _) =>
      An.error(SourceMessage(pos, "`::methods` of an alias isn't supported yet"))
    case _ => TypeInterpreter.instantiate(targetCon, paramCons.map(TypeCons.pseudoInstantiate), pos)
  }

  def methodDeclSection(section: Ast.MethodDeclSection): An[Env] =
    withTargetType(section.typeParams, section.pos) { (typeParamsEnv, paramCons, _) =>
      An.stepDeferring(section.methods)(env) {
        case (sectionEnv, Ast.MethodDecl(attributes, Ast.NameWithPos(name, namePos), typeExpr, _)) =>
          walkMethodDeclAttributes(attributes).zip(TypeExprAnalyzer.typeExprType(typeParamsEnv, typeExpr)).flatMap {
            case (methodAttributes, typ) =>
              val method = EnvMethod(methodAttributes, paramCons, typ, DefId(), env.nsPath)

              sectionEnv.addMethod(targetCon, name, method, namePos)
          }
      }
    }

  def methodDefSection(section: Ast.MethodDefSection): An[(Env, Terms.MethodDefSection)] = {
    val Ast.MethodDefSection(typeParams, receiver, methodDefs, pos) = section

    withTargetType(typeParams, pos) { (typeParamsEnv, paramCons, targetType) =>
      val receiverAn = PatternAnalyzer.walkParameter(typeParamsEnv, receiver, Some(targetType))

      receiverAn.flatMap { case (receiverEnv, receiverTerm) =>
        An.stepDeferring(methodDefs)((receiverEnv, Namespace.empty[Terms.MethodDef])) {
          case ((sectionEnv, terms), methodDef) => methodDefTerm(sectionEnv, methodDef).flatMap {
            case (methodAttributes, typ, bodyTerm) =>
              val Ast.NameWithPos(name, namePos) = methodDef.name
              val method = EnvMethod(methodAttributes, paramCons, typ, DefId(), env.nsPath)
              val term = bodyTerm.map(Terms.MethodDef(methodAttributes, _))

              sectionEnv.addMethod(targetCon, name, method, namePos).map { nextEnv =>
                (nextEnv, term.fold(terms)(term => terms.addOrModify((name, term))))
              }
          }
        }.map { case (sectionEnv, terms) =>
          (env.leaveMethodSection(sectionEnv), Terms.MethodDefSection(env.nsPath, receiverTerm, terms))
        }
      }
    }
  }

  def methodDefTerm(
    sectionEnv: Env,
    methodDef: Ast.MethodDef,
  ): An[(Attributes.MethodAttributes, Types.Type, Option[Terms.ValueExpr])] = {
    val Ast.MethodDef(attributes, _, typeExpr, body, _) = methodDef

    val signatureAn = typeExpr.map { typeExpr =>
      TypeExprAnalyzer.typeExprType(sectionEnv, typeExpr).map(typ => Some((typ, typeExpr.pos)))
    }.getOrElse(An.valid(None))

    walkMethodDefAttributes(attributes).zip(signatureAn).flatMap { case (methodAttributes, signature) =>
      val signatureType = signature.map(_._1)
      val bodyTermAn = ValueExprAnalyzer.valueExprTermExpecting(sectionEnv, body, signatureType).flatMap { bodyTerm =>
        signature.fold(An.valid(())) { case (typ, pos) =>
          TypeInterpreter.expectAssignable(bodyTerm.typ, typ, pos)
        }.map(_ => Option(bodyTerm))
      }

      An.deferErrors(bodyTermAn)(None).map { bodyTerm =>
        val typ = signatureType.orElse(bodyTerm.map(_.typ)).getOrElse(Types.ErrorType)

        (methodAttributes, typ, bodyTerm)
      }
    }
  }
}

object MethodSectionAnalyzer {
  def methodDeclSection(env: Env, section: Ast.MethodDeclSection): An[Env] =
    env.companionTypeCon.fold(An.valid(env))(MethodSectionAnalyzerImpl(env, _).methodDeclSection(section))

  def methodDefSection(
    env: Env,
    section: Ast.MethodDefSection,
  ): An[(Env, Option[Terms.MethodDefSection])] =
    env.companionTypeCon.fold(An.valid((env, Option.empty[Terms.MethodDefSection]))) { con =>
      MethodSectionAnalyzerImpl(env, con).methodDefSection(section).map { case (nextEnv, term) =>
        (nextEnv, Some(term))
      }
    }
}
