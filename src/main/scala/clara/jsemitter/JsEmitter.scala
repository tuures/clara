package clara.jsemitter

// Asg => JsAst

import clara.asg.{Attributes, TermLiteral, Terms, TypeCons, Namespace}

import impl._

import clara.util.Safe._


object JsEmitter {
  def emitModule(program: Terms.Block): JsAst.Module = {
    val body = program.bcs.flatMap(emitBlockContent)

    val moduleIntro = Seq(
      // JsAst.Const("$claraType", JsAst.UnaryCall(JsAst.Named("Symbol"), JsAst.StringLiteral("$claraType")))
    )

    JsAst.Module(moduleIntro ++ body)
  }

  private def illegal(term: Product): Nothing = throw new java.lang.AssertionError(safe"unexpected term: ${term.productPrefix}")

  def emitValueExpr(valueExpr: Terms.ValueExpr): JsAst.Expr = valueExpr match {
    case _: Terms.UnitLiteral => JsAst.Undefined
    case Terms.IntegerLiteral(value, _) => emitIntegerLiteral(value)
    case Terms.FloatLiteral(TermLiteral.Float(whole, fraction), _) => emitFloatLiteral(whole, fraction)
    case Terms.StringLiteral(parts, _) => emitStringLiteral(parts)
    case Terms.Tuple(es, _) => JsAst.ArrayLiteral(es.map(emitValueExpr))
    case Terms.Block(bcs, _) => emitBlock(bcs)
    case Terms.NamedValue(name, _) => JsAst.Named(name)
    case Terms.Record(fields, _) => JsAst.ObjectLiteral(fields.mapValues { case Terms.Field(body) =>
        emitValueExpr(body)
      }.entries)
    case Terms.Lambda(parameter, body, _) =>
      JsAst.UnaryArrowFunc(emitParameters(parameter), Seq(JsAst.Return(emitValueExpr(body))))
    case Terms.Piecewise(pieces, _) => emitPiecewise(pieces)
    case Terms.MemberSelection(obj, memberName, selectedMember, _) =>
      emitMemberSelection(obj, memberName, selectedMember)
    case Terms.Call(callee @ Terms.MemberSelection(obj, memberName, selectedMember, _), argument, _) =>
      selectedMember match {
        // TODO: is this branch needed or could be optimized on JsAst level after emit?
        case Terms.SelectedMethod(_, attributes) if attributes.emitKind.exists(_ === Attributes.BinaryOperator) =>
          emitBinaryOperation(obj, memberName, selectedMember, argument)
        case Terms.SelectedField =>
          emitCall(callee, argument)
      }
    case Terms.Call(callee, argument, _) => emitCall(callee, argument)
  }

  def emitIntegerLiteral(value: TermLiteral.Integer) = value match {
    case TermLiteral.IntegerBin(value) => JsAst.NumberLiteral(safe"0b$value")
    case TermLiteral.IntegerDec(value) => JsAst.NumberLiteral(value)
    case TermLiteral.IntegerHex(value) => JsAst.NumberLiteral(safe"0x$value")
  }

  def emitFloatLiteral(whole: String, fraction: String) = JsAst.NumberLiteral(safe"$whole.$fraction")

  def emitStringLiteral(parts: Seq[TermLiteral.StringValuePart]) = JsAst.StringLiteral(parts.map {
    case TermLiteral.StringPlainPart(value) => JsAst.StringPlainPart(value)
    case TermLiteral.StringEscapePart(escapes) => JsAst.StringEscapePart(escapes)
    case TermLiteral.StringValueExprPart(e) => JsAst.StringExpressionPart(emitValueExpr(e))
  })

  def emitBlock(bcs: Seq[Terms.BlockContent]): JsAst.Expr = {
    val emitted = bcs.flatMap(emitBlockContent)
    emitted match {
      // Single expression: no need for IIFE wrapper
      case Seq(e: JsAst.Expr) => e
      case _ =>
        // Wrap the last expression in Return, leave preceding ones as bare expressions (side effects)
        val body = emitted.lastOption match {
          case Some(e: JsAst.Expr) => emitted.init :+ JsAst.Return(e)
          case _ => emitted
        }
        JsAst.Iife(body)
    }
  }

  def emitPiecewise(pieces: Seq[(Terms.Pattern, Terms.ValueExpr)]): JsAst.Expr = {
    val WrapperParamName = "$value"
    def equals(v: JsAst.Expr) = JsAst.BinaryOperation(JsAst.Named(WrapperParamName), "===", v)

    def emitPiecewiseStringEquals(parts: Seq[TermLiteral.StringPatternPart]) = {
      val hasCaptures = parts.exists {
        case _: TermLiteral.StringCapturePart => true
        case _ => false
      }

      if (hasCaptures) {
        ??? // TODO: emit regex-based pattern matching for string captures
      } else {
        equals(JsAst.StringLiteral(parts.map {
          case TermLiteral.StringPlainPart(value) => JsAst.StringPlainPart(value)
          case TermLiteral.StringEscapePart(escapes) => JsAst.StringEscapePart(escapes)
          case TermLiteral.StringNamedConstantPart(Terms.NamedConstantPattern(namedValue)) =>
            JsAst.StringExpressionPart(emitValueExpr(namedValue))
          case capture: TermLiteral.StringCapturePart => illegal(capture)
        }))
      }
    }

    val (beforeWildcard, wildcardAndAfter) = pieces.span { case (pattern, _) =>
      !pattern.isInstanceOf[Terms.WildcardPattern]
    }

    val ifBranches = beforeWildcard.map { case (pattern, body) =>
      val predicate: JsAst.Expr = pattern match {
        case Terms.WildcardPattern(_) => illegal(pattern)
        case Terms.UnitPattern() => equals(JsAst.Undefined)
        case Terms.IntegerPattern(value, _) => equals(emitIntegerLiteral(value))
        case Terms.FloatPattern(TermLiteral.Float(whole, fraction), _) => equals(emitFloatLiteral(whole, fraction))
        case Terms.StringPattern(parts, _) => emitPiecewiseStringEquals(parts)
        case Terms.TuplePattern(_, _) => ???
        case Terms.CapturePattern(_, _) => ???
        case Terms.NamedConstantPattern(namedValue) => equals(emitValueExpr(namedValue))
      }
      JsAst.IfBranch(predicate, Seq(JsAst.Return(emitValueExpr(body))))
    }

    val elseBranch = wildcardAndAfter.headOption.map { case (_, body) =>
      Seq(JsAst.Return(emitValueExpr(body)))
    }.getOrElse(Nil)

    JsAst.UnaryArrowFunc(JsAst.NamePattern(WrapperParamName), Seq(JsAst.IfElse(ifBranches, elseBranch)))
  }

  def emitBlockContent(blockContent: Terms.BlockContent): Option[JsAst.Content] = blockContent match {
    case e: Terms.ValueExpr => Some(emitValueExpr(e))
    case _: Terms.ValueDecl => None
    case Terms.ValueDef(target, e) => Some(JsAst.Const(emitValueDefTarget(target), emitValueExpr(e)))
    case Terms.TypeDef(con) => con match {
      case TypeCons.SingletonTypeCon(name, _, _) => Some(JsAst.Const(
        JsAst.NamePattern(name),
        JsAst.StringLiteral(name) // TODO object? symbol?
      ))
      case _ => None
    }
    case _: Terms.MethodDeclSection => None
    case Terms.MethodDefSection(targetCon, selfPattern, methodDefs) =>
      Some(emitMethodDefSection(targetCon, selfPattern, methodDefs))
  }

  def emitValueDefTarget(target: Terms.Pattern): JsAst.Pattern = target match {
    case Terms.WildcardPattern(_) => JsAst.NamePattern("_")
    case Terms.UnitPattern() => JsAst.UnitPattern
    case Terms.IntegerPattern(_, _) => ???
    case Terms.FloatPattern(_, _) => ???
    case Terms.StringPattern(_, _) => ???
    case Terms.TuplePattern(ps, _) => JsAst.ArrayPattern(ps.map(emitValueDefTarget))
    case Terms.CapturePattern(name, _) => JsAst.NamePattern(name)
    case Terms.NamedConstantPattern(_) => ???
  }

  def emitMethodDefSection(targetCon: TypeCons.TypeCon, selfPattern: Terms.Pattern, methodDefs: Namespace[Terms.MethodDef]) = {
    val entries = methodDefs.mapValues { case Terms.MethodDef(attributes, body) =>
      JsAst.UnaryArrowFunc(emitParameters(selfPattern), Seq(JsAst.Return(emitValueExpr(body))))
    }.entries

    JsAst.Const(
      JsAst.NamePattern(NameMangler.methodsCompanionName(targetCon)),
      JsAst.ObjectLiteral(entries)
    )
  }

  def emitMemberName(memberName: String, selectedMember: Terms.SelectedMember): String = {
    val nameOverride = selectedMember match {
      case Terms.SelectedMethod(_, attributes) => attributes.emitName
      case Terms.SelectedField => None
    }

    nameOverride.getOrElse(memberName)
  }

  def emitMemberSelection(obj: Terms.ValueExpr, memberName: String, selectedMember: Terms.SelectedMember) = {
    val name = emitMemberName(memberName, selectedMember)

    def selectInstanceProperty = JsAst.Member(emitValueExpr(obj), name)

    selectedMember match {
      case Terms.SelectedMethod(targetCon, attributes) => attributes.emitKind match {
        case Some(Attributes.InstanceProperty) => selectInstanceProperty
        case Some(Attributes.BinaryOperator) =>
          JsAst.UnaryArrowFunc(JsAst.NamePattern("_"), Seq(JsAst.Return(JsAst.BinaryOperation(emitValueExpr(obj), name, JsAst.Named("_")))))
        case None => JsAst.UnaryCall(JsAst.Member(JsAst.Named(NameMangler.methodsCompanionName(targetCon)), name), emitValueExpr(obj))
      }
      case Terms.SelectedField => selectInstanceProperty
    }
  }

  def emitBinaryOperation(obj: Terms.ValueExpr, memberName: String, selectedMember: Terms.SelectedMember, argument: Terms.ValueExpr) =
    JsAst.BinaryOperation(emitValueExpr(obj), emitMemberName(memberName, selectedMember), emitValueExpr(argument))

  def emitCall(callee: Terms.ValueExpr, argument: Terms.ValueExpr) =
    JsAst.UnaryCall(emitValueExpr(callee), emitValueExpr(argument))

  def emitParameters(pattern: Terms.Pattern): JsAst.Pattern = pattern match {
    case Terms.WildcardPattern(_) => ???
    case Terms.UnitPattern() => JsAst.UnitPattern
    case Terms.IntegerPattern(_, _) => ???
    case Terms.FloatPattern(_, _) => ???
    case Terms.StringPattern(_, _) => ???
    case Terms.TuplePattern(ps, _) => JsAst.ArrayPattern(ps.map(emitParameters))
    case Terms.CapturePattern(name, _) => JsAst.NamePattern(name)
    case Terms.NamedConstantPattern(_) => ???
  }
}

object NameMangler {
  def methodsCompanionName(targetCon: TypeCons.TypeCon) = safe"${targetCon.name}$$Methods"
}
