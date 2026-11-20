package clara.analyzer.impl

import clara.asg.{Attributes, DefId, Terms, Types, TypeCons, Namespace}
import clara.ast.{Ast, Pos, SourceMessage}

import clara.util.Safe._


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

  def targetType(paramCons: Seq[TypeCons.ParamCon], pos: Pos): An[Types.Type] =
    TypeInterpreter.instantiate(targetCon, paramCons.map(TypeCons.pseudoInstantiate), pos)

  def targets(targetType: Types.Type, pos: Pos): An[Seq[(TypeCons.TypeCon, Types.Type)]] = targetType match {
    case alias: Types.Alias => memberTargets(alias, alias, pos)
    case Types.ErrorType => TypeCons.pseudoInstantiate(targetCon) match {
      case alias: Types.Alias => memberTargets(alias, alias, pos).map(_.map { case (con, _) => (con, Types.ErrorType) })
      case _ => An.valid(Seq((targetCon, Types.ErrorType)))
    }
    case _ => An.valid(Seq((targetCon, targetType)))
  }

  def memberTargets(alias: Types.Alias, typ: Types.Type, pos: Pos): An[Seq[(TypeCons.TypeCon, Types.Type)]] = {
    lazy val cannotHaveMethods = An.error(SourceMessage(pos,
      safe"`::methods` of `${Types.toSource(alias)}` would add methods to `${Types.toSource(typ)}`, " +
        "only tagged, boxed, opaque and singleton types can have methods"
    ))

    typ match {
      case t: Types.Alias => TypeInterpreter.wrappedType(t).flatMap(memberTargets(alias, _, pos))
      case Types.Union(alternatives) => An.seq(alternatives.map(memberTargets(alias, _, pos))).map(_.flatten)
      case _: Types.Param => cannotHaveMethods
      case t: Types.Nominal => An.deferErrors(rejectInstantiation(alias, t, pos))(()).map(_ => Seq((t.con, t)))
      case _ => cannotHaveMethods
    }
  }

  def rejectInstantiation(alias: Types.Alias, target: Types.Nominal, pos: Pos): An[Unit] = {
    val sectionTypeParamIds = alias.typeArgs.collect { case p: Types.Param => p.con.tcId }
    val typeArgs = target match {
      case t: Types.Wrapper => t.typeArgs
      case t: Types.Opaque => t.typeArgs
      case _: Types.Singleton | _: Types.Param => Nil
    }
    val typeArgParamIds = typeArgs.collect {
      case p: Types.Param if sectionTypeParamIds.contains(p.con.tcId) => p.con.tcId
    }
    val isEveryInstantiation = typeArgParamIds.distinct.length === typeArgs.length

    An.errorIf(!isEveryInstantiation)(SourceMessage(pos,
      safe"`::methods` of `${Types.toSource(alias)}` would only apply to `${Types.toSource(target)}`, " +
        safe"not to every `${TypeCons.toSource(target.con)}`, which isn't supported yet"
    ))
  }

  def addMethod(sectionEnv: Env, targets: Seq[(TypeCons.TypeCon, Types.Type)], name: String, pos: Pos)(
    method: Types.Type => EnvMethod
  ): An[Env] =
    An.step(targets)(sectionEnv) { case (nextEnv, (con, targetType)) =>
      nextEnv.addMethod(con, name, method(targetType), pos)
    }

  def methodDeclSection(section: Ast.MethodDeclSection): An[Env] = targetCon match {
    case TypeCons.WrapperTypeCon(Ast.TypeDefKind.Alias, _, _, _, _, _) =>
      An.error(SourceMessage(section.pos, "`::declare ::methods` of an alias isn't supported yet"))
    case _ => withTargetType(section.typeParams, section.pos) { (typeParamsEnv, paramCons, targetType) =>
      An.stepDeferring(section.methods)(env) {
        case (sectionEnv, Ast.MethodDecl(attributes, Ast.NameWithPos(name, namePos), typeExpr, _)) =>
          walkMethodDeclAttributes(attributes).zip(TypeExprAnalyzer.typeExprType(typeParamsEnv, typeExpr)).flatMap {
            case (methodAttributes, typ) =>
              val defId = DefId()

              addMethod(sectionEnv, Seq((targetCon, targetType)), name, namePos) {
                EnvMethod(methodAttributes, paramCons, _, typ, defId, env.nsPath)
              }
          }
      }
    }
  }

  def methodDefSection(section: Ast.MethodDefSection): An[(Env, Terms.MethodDefSection)] = {
    val Ast.MethodDefSection(typeParams, receiver, methodDefs, pos) = section

    withTargetType(typeParams, pos) { (typeParamsEnv, paramCons, targetType) =>
      val targetsAn = An.deferErrors(targets(targetType, pos))(Seq((targetCon, targetType)))
      val receiverAn = PatternAnalyzer.walkParameter(typeParamsEnv, receiver, Some(targetType))

      targetsAn.zip(receiverAn).flatMap { case (targets, (receiverEnv, receiverTerm)) =>
        An.stepDeferring(methodDefs)((receiverEnv, Namespace.empty[Terms.MethodDef])) {
          case ((sectionEnv, terms), methodDef) => methodDefTerm(sectionEnv, methodDef).flatMap {
            case (methodAttributes, typ, bodyTerm) =>
              val Ast.NameWithPos(name, namePos) = methodDef.name
              val defId = DefId()
              val term = bodyTerm.map(Terms.MethodDef(methodAttributes, _))

              addMethod(sectionEnv, targets, name, namePos) {
                EnvMethod(methodAttributes, paramCons, _, typ, defId, env.nsPath)
              }.map { nextEnv =>
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
