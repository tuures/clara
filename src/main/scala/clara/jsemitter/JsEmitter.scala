package clara.jsemitter

// Asg => JsAst

import clara.asg.{Attributes, Terms, Types, Namespace}
import clara.ast.LiteralValue

import impl._

import ai.x.safe._


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
    case Terms.Block(bcs, _) => emitBlock(bcs)
    case Terms.NamedValue(name, _) => JsAst.Named(name)
    case Terms.Record(fields, _) => JsAst.ObjectLiteral(fields.mapValues { case Terms.Field(body) =>
        emitValueExpr(body)
      }.entries)
    case Terms.MemberSelection(obj, memberName, selectedMember, _) =>
      emitMemberSelection(obj, memberName, selectedMember)
    case Terms.Call(callee @ Terms.MemberSelection(obj, memberName, selectedMember, _), argument, _) =>
      selectedMember match {
        case Terms.SelectedMethod(attributes) if attributes.emitKind.exists(_ === Attributes.BinaryOperator) =>
          emitBinaryOperation(obj, memberName, selectedMember, argument)
        case Terms.SelectedField =>
          emitCall(callee, argument)
      }
    case Terms.Call(callee, argument, _) => emitCall(callee, argument)
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
  }).safeMkString(""))

  def emitBlock(bcs: Seq[Terms.BlockContent]) = {
    JsAst.NullaryCall(JsAst.NullaryArrowFunc(bcs.flatMap(emitBlockContent)))
  }

  def emitBlockContent(blockContent: Terms.BlockContent): Option[JsAst.Node] = blockContent match {
    case e: Terms.ValueExpr => Some(emitValueExpr(e))
    case Terms.ValueNamesDef(target, e) => Some(emitValueNamesDef(target, e))
    case _: Terms.TypeDef => None
    case _: Terms.MethodDeclSection => None
    case Terms.MethodDefSection(targetType, selfPattern, methodDefs) => Some(emitMethodDefSection(targetType, selfPattern, methodDefs))
  }

  def emitValueNamesDef(target: Terms.Pattern, e: Terms.ValueExpr) = target match {
    case Terms.NamePattern(name) => JsAst.Const(name, emitValueExpr(e))
  }

  def emitMethodDefSection(targetType: Types.Typ, selfPattern: Terms.Pattern, methodDefs: Namespace[Terms.MethodDef]) = {
    val entries = methodDefs.mapValues { case Terms.MethodDef(attributes, body) =>
      emitValueExpr(body)
    }.entries

    // FIXME func from self pattern to object
    JsAst.Const(NameMangler.methodsCompanionName(targetType), JsAst.ObjectLiteral(entries))
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
        case Some(Attributes.BinaryOperator) => JsAst.UnaryArrowFunc("_", Seq(JsAst.BinaryOperation(name, emitValueExpr(obj), JsAst.Named("_"))))
        case None => JsAst.UnaryCall(JsAst.Member(JsAst.Named(NameMangler.methodsCompanionName(obj.typ)), name), emitValueExpr(obj))
      }
      case Terms.SelectedField => selectInstanceProperty
    }
  }

  def emitBinaryOperation(obj: Terms.ValueExpr, memberName: String, selectedMember: Terms.SelectedMember, argument: Terms.ValueExpr) =
    JsAst.BinaryOperation(emitMemberName(memberName, selectedMember), emitValueExpr(obj), emitValueExpr(argument))

  def emitCall(callee: Terms.ValueExpr, argument: Terms.ValueExpr) =
    JsAst.UnaryCall(emitValueExpr(callee), emitValueExpr(argument))
}

object NameMangler {
  def methodsCompanionName(typ: Types.Typ) = safe"${typeName(typ)}$$Methods"

  def typeName(typ: Types.Typ) = typ match {
    case u: Types.Unique => u.name
    case _ => ???
  }
}
