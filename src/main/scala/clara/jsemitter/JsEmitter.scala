package clara.jsemitter

// Asg => JsAst

import clara.asg.{Attributes, TermLiteral, Terms, TypeCons, Namespace}
import clara.ast.Ast.TypeDefKind

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
    case Terms.NamedValue(name, definedInNs, _) => JsAst.Named(NameMangler.valueName(name, definedInNs))
    case Terms.ValueAs(term, _) => emitValueExpr(term)
    case Terms.Record(fields, _) => JsAst.ObjectLiteral(fields.mapValues { case Terms.Field(body) =>
        emitValueExpr(body)
      }.entries)
    case Terms.Lambda(parameter, body, _) =>
      JsAst.UnaryArrowFunc(emitBindingPattern(parameter, Nil), Seq(JsAst.Return(emitValueExpr(body))))
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

    val cases = pieces.map { case (pattern, body) => (emitCaseMatch(pattern, JsAst.Named(WrapperParamName)), body) }
    // the first case without conditions matches every value, cases after it are unreachable
    val (conditionalCases, unconditionalAndAfter) = cases.span { case (caseMatch, _) => caseMatch.conditions.nonEmpty }

    def caseBody(caseMatch: CaseMatch, body: Terms.ValueExpr): Seq[JsAst.Content] =
      caseMatch.bindings.map { case (name, expr) => JsAst.Const(JsAst.NamePattern(NameMangler.valueName(name, Nil)), expr) } :+
        JsAst.Return(emitValueExpr(body))

    val ifBranches = conditionalCases.map { case (caseMatch, body) =>
      JsAst.IfBranch(caseMatch.conditions.reduceLeft(JsAst.BinaryOperation(_, "&&", _)), caseBody(caseMatch, body))
    }
    val elseBranch = unconditionalAndAfter.headOption.map { case (caseMatch, body) => caseBody(caseMatch, body) }.getOrElse(Nil)

    val funcBody = if (ifBranches.isEmpty) elseBranch else Seq(JsAst.IfElse(ifBranches, elseBranch))

    JsAst.UnaryArrowFunc(JsAst.NamePattern(WrapperParamName), funcBody)
  }

  case class CaseMatch(conditions: Seq[JsAst.Expr], bindings: Seq[(String, JsAst.Expr)])

  def emitCaseMatch(pattern: Terms.Pattern, value: JsAst.Expr): CaseMatch = {
    def equals(expected: JsAst.Expr) = CaseMatch(Seq(JsAst.BinaryOperation(value, "===", expected)), Nil)

    pattern match {
      case Terms.WildcardPattern(_) => CaseMatch(Nil, Nil)
      case Terms.UnitPattern() => equals(JsAst.Undefined)
      case Terms.IntegerPattern(literal, _) => equals(emitIntegerLiteral(literal))
      case Terms.FloatPattern(TermLiteral.Float(whole, fraction), _) => equals(emitFloatLiteral(whole, fraction))
      // a single capture matches every string, the analyzer rejects it where it would need a runtime type test
      case Terms.StringPattern(Seq(TermLiteral.StringCapturePart(Terms.CapturePattern(name, _))), _) =>
        CaseMatch(Nil, Seq(name -> value))
      case Terms.StringPattern(parts, _) => equals(JsAst.StringLiteral(parts.map {
        case TermLiteral.StringPlainPart(plain) => JsAst.StringPlainPart(plain)
        case TermLiteral.StringEscapePart(escapes) => JsAst.StringEscapePart(escapes)
        case TermLiteral.StringNamedConstantPart(Terms.NamedConstantPattern(namedValue)) =>
          JsAst.StringExpressionPart(emitValueExpr(namedValue))
        case _: TermLiteral.StringCapturePart => ??? // TODO: string captures need runtime matching
      }))
      case Terms.TuplePattern(ps, _) =>
        val elements = ps.zipWithIndex.map { case (p, i) =>
          emitCaseMatch(p, JsAst.Index(value, JsAst.NumberLiteral(i.toString)))
        }
        // TODO skip the array checks when the matched type is known to be a tuple of this length
        val isTupleOfLength = Seq(
          JsAst.UnaryCall(JsAst.Member(JsAst.Member(JsAst.Named("globalThis"), "Array"), "isArray"), value),
          JsAst.BinaryOperation(JsAst.Member(value, "length"), "===", JsAst.NumberLiteral(ps.length.toString)),
        )
        CaseMatch(isTupleOfLength ++ elements.flatMap(_.conditions), elements.flatMap(_.bindings))
      case Terms.NamedConstantPattern(namedValue) => equals(emitValueExpr(namedValue))
      case Terms.CapturePattern(name, _) => CaseMatch(Nil, Seq(name -> value))
      // the analyzer rejects type patterns in cases where they would need a runtime type test
      case Terms.PatternAs(p, _) => emitCaseMatch(p, value)
    }
  }

  def emitBlockContent(blockContent: Terms.BlockContent): Seq[JsAst.Content] = blockContent match {
    case e: Terms.ValueExpr => Seq(emitValueExpr(e))
    case _: Terms.ValueDecl => Nil
    case Terms.ValueDef(target, definedInNs, e) => Seq(JsAst.Const(emitBindingPattern(target, definedInNs), emitValueExpr(e)))
    case Terms.TypeDef(con) => con match {
      // a unique object, so values compare by identity, the tag is only for debugging and serialization
      case TypeCons.SingletonTypeCon(name, definedInNs, _, _) => Seq(JsAst.Const(
        JsAst.NamePattern(NameMangler.valueName(name, definedInNs)),
        JsAst.ObjectLiteral(Seq(NameMangler.TypeTagField -> typeTag(name, definedInNs)))
      ))
      case TypeCons.WrapperTypeCon(TypeDefKind.Tagged, name, _, definedInNs, _, _) =>
        Seq(JsAst.Const(JsAst.NamePattern(constructorName(name, definedInNs)), constructor(identity)))
      case TypeCons.WrapperTypeCon(TypeDefKind.Boxed, name, _, definedInNs, _, _) =>
        Seq(JsAst.Const(JsAst.NamePattern(constructorName(name, definedInNs)), constructor(value => JsAst.ObjectLiteral(Seq(
          NameMangler.TypeTagField -> typeTag(name, definedInNs),
          NameMangler.BoxedValueField -> value,
        )))))
      case _ => Nil
    }
    case _: Terms.MethodDeclSection => Nil
    case Terms.MethodDefSection(targetCon, selfPattern, methodDefs) =>
      Seq(emitMethodDefSection(targetCon, selfPattern, methodDefs))
  }

  def constructorName(name: String, definedInNs: Seq[String]): String =
    NameMangler.valueName(TypeCons.ConstructorName, definedInNs :+ name)

  def typeTag(name: String, definedInNs: Seq[String]): JsAst.Expr = JsAst.StringLiteral((definedInNs :+ name).safeString("."))

  def constructor(construct: JsAst.Expr => JsAst.Expr): JsAst.Expr = {
    val ParamName = "$value"
    JsAst.UnaryArrowFunc(JsAst.NamePattern(ParamName), Seq(JsAst.Return(construct(JsAst.Named(ParamName)))))
  }

  def emitMethodDefSection(targetCon: TypeCons.TypeCon, selfPattern: Terms.Pattern, methodDefs: Namespace[Terms.MethodDef]) = {
    val entries = methodDefs.mapValues { case Terms.MethodDef(attributes, body) =>
      JsAst.UnaryArrowFunc(emitBindingPattern(selfPattern, Nil), Seq(JsAst.Return(emitValueExpr(body))))
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

  def emitBindingPattern(pattern: Terms.Pattern, definedInNs: Seq[String]): JsAst.Pattern = pattern match {
    case Terms.WildcardPattern(_) => JsAst.IgnoredPattern
    case Terms.UnitPattern() => JsAst.IgnoredPattern
    // the analyzer rejects literal patterns outside the cases of a piecewise function
    case _: Terms.IntegerPattern => illegal(pattern)
    case _: Terms.FloatPattern => illegal(pattern)
    case Terms.StringPattern(Seq(TermLiteral.StringCapturePart(Terms.CapturePattern(name, _))), _) =>
      JsAst.NamePattern(NameMangler.valueName(name, definedInNs))
    case _: Terms.StringPattern => illegal(pattern)
    case Terms.TuplePattern(ps, _) => JsAst.ArrayPattern(ps.map(emitBindingPattern(_, definedInNs)))
    case Terms.CapturePattern(name, _) => JsAst.NamePattern(NameMangler.valueName(name, definedInNs))
    case Terms.NamedConstantPattern(_) => JsAst.IgnoredPattern
    case Terms.PatternAs(p, _) => emitBindingPattern(p, definedInNs)
  }

}

object NameMangler {
  private val JsKeywords = Set(
    "await", "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else",
    "enum", "export", "extends", "false", "finally", "for", "function", "if", "implements", "import", "in",
    "instanceof", "interface", "let", "new", "null", "package", "private", "protected", "public", "return", "static",
    "super", "switch", "this", "throw", "true", "try", "typeof", "var", "void", "while", "with", "yield",
    "arguments", "eval", "undefined",
  )

  // globalThis must not be shadowed so that the emitted code can safely reference it
  val EscapeNames = JsKeywords ++ Set("globalThis")

  val TypeTagField = "$type"
  val BoxedValueField = "$value"

  def valueName(name: String, definedInNs: Seq[String]) =
    if (definedInNs.nonEmpty) (definedInNs :+ name).safeString("$")
    else if (EscapeNames.contains(name)) safe"$name$$"
    else name

  def methodsCompanionName(targetCon: TypeCons.TypeCon) = safe"${targetCon.name}$$Methods"
}
