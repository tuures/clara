package clara.jsemitter

// Asg => JsAst

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.LiteralValue

import impl._

import clara.util.Safe._


object JsEmitter {
  def emitProgram(program: Terms.Block): JsAst.Module = {
    val body = program.bcs.flatMap(emitBlockContent)

    // JsAst.UnaryCall(JsAst.UnaryArrowFunc("$", body), JsAst.Named("global"))
    val helpers = Seq(
      // JsAst.Const("$claraType", JsAst.UnaryCall(JsAst.Named("Symbol"), JsAst.StringLiteral("$claraType")))
    )

    JsAst.Module(helpers ++ body)
  }

  def emitValueExpr(valueExpr: Terms.ValueExpr): JsAst.Expr = valueExpr match {
    case _: Terms.UnitLiteral => JsAst.Undefined
    case Terms.IntegerLiteral(value, _) => emitIntegerLiteral(value)
    case Terms.FloatLiteral(LiteralValue.Float(whole, fraction), _) => emitFloatLiteral(whole, fraction)
    case Terms.StringLiteral(parts, _) => emitStringLiteral(parts)
    case Terms.Tuple(es, _) => JsAst.ArrayLiteral(es.map(emitValueExpr))
    case Terms.Block(bcs, _) => emitBlock(bcs)
    case Terms.NamedValue(name, _) => JsAst.Named(name)
    case Terms.Record(fields, _) => JsAst.ObjectLiteral(fields.mapValues { case Terms.Field(body) =>
        emitValueExpr(body)
      }.entries)
    case Terms.Lambda(parameter, body, _) => JsAst.UnaryArrowFunc(emitParameters(parameter), Seq(emitValueExpr(body)))
    case Terms.Piecewise(pieces, _) => {
      val ifBranches = pieces.map { case (pattern, body) =>
        val predicateExpr = pattern match {
          case Terms.CapturePattern(name, typ) => ???
          case Terms.LiteralPattern(term) => JsAst.BinaryOperation("===", JsAst.Named("$value"), emitValueExpr(term))
          case Terms.TuplePattern(ps, typ) => ???
          case Terms.UnitPattern() => ???
        }

        JsAst.IfBranch(predicateExpr, Seq(emitValueExpr(body)))
      }

      JsAst.UnaryArrowFunc(JsAst.NamePattern("$value"), Seq(JsAst.If(ifBranches, Nil)))
    }
    case Terms.MemberSelection(obj, memberName, selectedMember, _) =>
      emitMemberSelection(obj, memberName, selectedMember)
    case Terms.Call(callee @ Terms.MemberSelection(obj, memberName, selectedMember, _), argument, _) =>
      selectedMember match {
        case Terms.SelectedMethod(attributes) if attributes.emitKind.exists(_ === Attributes.BinaryOperator) =>
          emitBinaryOperation(obj, memberName, selectedMember, argument)
        case Terms.SelectedField =>
          emitCall(callee, argument)
      }
    case Terms.Call(_: Terms.NewExpr, argument, _) => emitValueExpr(argument)
    case Terms.Call(callee, argument, _) => emitCall(callee, argument)
    case Terms.NewExpr(_) => JsAst.UnaryArrowFunc(JsAst.NamePattern("_"), Seq(JsAst.Named("_"))) // TODO JsAst.Iife
  }

  def emitIntegerLiteral(value: LiteralValue.Integer) = value match {
    case LiteralValue.IntegerBin(value) => ???
    case LiteralValue.IntegerDec(value) => JsAst.NumberLiteral(value)
    case LiteralValue.IntegerHex(value) => ???
  }

  def emitFloatLiteral(whole: String, fraction: String) = JsAst.NumberLiteral(safe"$whole.$fraction")

  def emitStringLiteral(parts: Seq[LiteralValue.StringPart]) = JsAst.StringLiteral((parts.map {
    case LiteralValue.StringEscapePart(escapes) => ???
    case LiteralValue.StringExpressionPart(e) => ???
    case LiteralValue.StringPlainPart(value) => value
  }).safeString(""))

  def emitBlock(bcs: Seq[Terms.BlockContent]) = {
    // TODO: JsAst.Iife so that we can automatically optimise unintentional iifes away
    JsAst.NullaryCall(JsAst.NullaryArrowFunc(bcs.flatMap(emitBlockContent)))
  }

  def emitBlockContent(blockContent: Terms.BlockContent): Option[JsAst.Content] = blockContent match {
    case e: Terms.ValueExpr => Some(emitValueExpr(e))
    case _: Terms.ValueDecl => None
    case Terms.ValueDef(target, e) => Some(JsAst.Const(emitValueDefTarget(target), emitValueExpr(e)))
    case _: Terms.AliasTypeDef => None
    case _: Terms.TypeDef => None
    case _: Terms.MethodDeclSection => None
    case Terms.MethodDefSection(targetType, selfPattern, methodDefs) => Some(emitMethodDefSection(targetType, selfPattern, methodDefs))
  }

  def emitValueDefTarget(target: Terms.Pattern): JsAst.Pattern = target match {
    case Terms.UnitPattern() => ???
    case Terms.TuplePattern(ps, _) => JsAst.ArrayPattern(ps.map(emitValueDefTarget))
    case Terms.CapturePattern(name, _) => JsAst.NamePattern(name)
  }

  def emitMethodDefSection(targetType: Types.Type, selfPattern: Terms.Pattern, methodDefs: Namespace[Terms.MethodDef]) = {
    val entries = methodDefs.mapValues { case Terms.MethodDef(attributes, body) =>
      JsAst.UnaryArrowFunc(emitParameters(selfPattern), Seq(emitValueExpr(body)))
    }.entries

    JsAst.Const(
      JsAst.NamePattern(NameMangler.methodsCompanionName(targetType)),
      JsAst.ObjectLiteral(entries)
    )
  }

  def emitMemberName(memberName: String, selectedMember: Terms.SelectedMember): String = {
    val nameOverride = selectedMember match {
      case Terms.SelectedMethod(attributes) => attributes.emitName
      case Terms.SelectedField => None
    }

    nameOverride.getOrElse(memberName)
  }

  def emitMemberSelection(obj: Terms.ValueExpr, memberName: String, selectedMember: Terms.SelectedMember) = {
    val name = emitMemberName(memberName, selectedMember)

    def selectInstanceProperty = JsAst.Member(emitValueExpr(obj), name)

    selectedMember match {
      case Terms.SelectedMethod(attributes) => attributes.emitKind match {
        case Some(Attributes.InstanceProperty) => selectInstanceProperty
        case Some(Attributes.BinaryOperator) => JsAst.UnaryArrowFunc(JsAst.NamePattern("_"), Seq(JsAst.BinaryOperation(name, emitValueExpr(obj), JsAst.Named("_"))))
        case None => JsAst.UnaryCall(JsAst.Member(JsAst.Named(NameMangler.methodsCompanionName(obj.typ)), name), emitValueExpr(obj))
      }
      case Terms.SelectedField => selectInstanceProperty
    }
  }

  def emitBinaryOperation(obj: Terms.ValueExpr, memberName: String, selectedMember: Terms.SelectedMember, argument: Terms.ValueExpr) =
    JsAst.BinaryOperation(emitMemberName(memberName, selectedMember), emitValueExpr(obj), emitValueExpr(argument))

  def emitCall(callee: Terms.ValueExpr, argument: Terms.ValueExpr) =
    JsAst.UnaryCall(emitValueExpr(callee), emitValueExpr(argument))

  def emitParameters(pattern: Terms.Pattern): JsAst.Pattern = pattern match {
    case Terms.UnitPattern() => JsAst.UnitPattern
    case Terms.TuplePattern(ps, _) => JsAst.ArrayPattern(ps.map(emitParameters))
    case Terms.CapturePattern(name, _) => JsAst.NamePattern(name)
  }
}

object NameMangler {
  def methodsCompanionName(typ: Types.Type) = safe"${typeName(typ)}$$Methods"

  def typeName(typ: Types.Type) = typ match {
    // case Types.Alias(name, _) => name
    case t => throw new scala.NotImplementedError(t.toString())
  }
}
