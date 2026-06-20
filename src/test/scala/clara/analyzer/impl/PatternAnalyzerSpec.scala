package clara.analyzer.impl

import clara.ast.{Ast, AstLiteral}
import clara.asg.{Namespace, TermLiteral, Terms, Types}
import clara.testutil.{AnalyzerTestPrelude, BaseSpec}

class PatternAnalyzerSpec extends BaseSpec {
  private val testEnv = AnalyzerTestPrelude.env

  // TODO: move these to some test utility object since they could be used in multiple places
  private def assertAnSuccess[A](result: An[A], expected: A): Unit = {
    val _ = assert(result.value.value === expected)
  }

  private def assertNotAssignableError(result: An[_]): Unit = {
    assertAnSingleErrorContains(result, "is not assignable")
  }

  private def assertAnSingleErrorContains(result: An[_], expectedText: String): Unit = {
    val _ = inside(result.value) {
      case Left(Seq(error)) =>
        assert(error.message.contains(expectedText))
    }
  }

  test("WildcardPattern with unknown source type should use Top") {
    val result = PatternAnalyzer(Env.empty, Env.empty).walkAssignment(Ast.WildcardPattern(), None)

    assertAnSuccess(result, (Env.empty, Terms.WildcardPattern(Types.Top)))
  }

  test("WildcardPattern with known source type should keep that type") {
    val result = PatternAnalyzer(Env.empty, Env.empty).walkAssignment(Ast.WildcardPattern(), Some(Types.Uni))

    assertAnSuccess(result, (Env.empty, Terms.WildcardPattern(Types.Uni)))
  }

  test("UnitPattern should succeed with unknown source type") {
    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(Ast.UnitPattern(), None)

    assertAnSuccess(result, (testEnv, Terms.UnitPattern()))
  }

  test("UnitPattern should fail when source type is incompatible") {
    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(Ast.UnitPattern(), Some(AnalyzerTestPrelude.intType))

    assertNotAssignableError(result)
  }

  test("IntegerPattern should resolve Int and return typed pattern") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSuccess(result, (testEnv, Terms.IntegerPattern(TermLiteral.IntegerDec("42"), AnalyzerTestPrelude.intType)))
  }

  test("IntegerPattern should fail when source type is incompatible") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(Types.Uni))

    assertNotAssignableError(result)
  }

  test("FloatPattern should resolve Float and return typed pattern") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSuccess(result, (testEnv, Terms.FloatPattern(TermLiteral.Float("3", "14"), AnalyzerTestPrelude.floatType)))
  }

  test("FloatPattern should fail when source type is incompatible") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(Types.Uni))

    assertNotAssignableError(result)
  }

  test("StringPattern should resolve String and return typed pattern") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSuccess(result, (testEnv, Terms.StringPattern(Seq(TermLiteral.StringPlainPart("hello")), AnalyzerTestPrelude.stringType)))
  }

  test("StringPattern should fail when source type is incompatible") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(Types.Uni))

    assertNotAssignableError(result)
  }

  test("StringPattern should handle escape sequences, captures and named constants") {
    val envWithConstant = testEnv.copy(values = Namespace(("ConstStr", AnalyzerTestPrelude.stringType)))
    val pattern = Ast.StringPattern(Seq(
      AstLiteral.StringPlainPart("hello "),
      AstLiteral.StringCapturePart(Ast.CapturePattern("captured")),
      AstLiteral.StringNamedConstantPart(Ast.NamedConstantPattern("ConstStr")),
      AstLiteral.StringEscapePart(Seq("n"))
    ))

    val result = PatternAnalyzer(envWithConstant, Env.empty).walkAssignment(pattern, None)

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(term.isInstanceOf[Terms.StringPattern])
        assert(nextEnv.values.get("captured") === Some(AnalyzerTestPrelude.stringType))
    }
  }

  test("TuplePattern should succeed with unknown source type") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.IntegerPattern(AstLiteral.IntegerDec("42"))))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    val expectedTerm = Terms.TuplePattern(
      Seq(Terms.UnitPattern(), Terms.IntegerPattern(TermLiteral.IntegerDec("42"), AnalyzerTestPrelude.intType)),
      Types.Tuple(Seq(Types.Uni, AnalyzerTestPrelude.intType))
    )
    assertAnSuccess(result, (testEnv, expectedTerm))
  }

  test("TuplePattern should fail when source type is not a tuple") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.IntegerPattern(AstLiteral.IntegerDec("42"))))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(AnalyzerTestPrelude.intType))

    assertNotAssignableError(result)
  }

  test("TuplePattern should fail when source tuple has incompatible element types") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.IntegerPattern(AstLiteral.IntegerDec("42"))))
    val fromType = Types.Tuple(Seq(Types.Uni, Types.Uni))

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(fromType))

    assertNotAssignableError(result)
  }

  test("NamedConstantPattern should resolve named value from environment") {
    val envWithValue = testEnv.copy(values = Namespace(("Const1", AnalyzerTestPrelude.intType)))
    val pattern = Ast.NamedConstantPattern("Const1")

    val result = PatternAnalyzer(envWithValue, Env.empty).walkAssignment(pattern, None)

    assertAnSuccess(result, (
      envWithValue,
      Terms.NamedConstantPattern(Terms.NamedValue("Const1", AnalyzerTestPrelude.intType))
    ))
  }

  test("NamedConstantPattern should fail for unknown value") {
    val pattern = Ast.NamedConstantPattern("MissingValue")

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSingleErrorContains(result, "Unknown value")
  }

  test("CapturePattern should bind captured name with inferred source type") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(AnalyzerTestPrelude.intType))

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(nextEnv.values.get("captured") === Some(AnalyzerTestPrelude.intType))
        assert(term === Terms.CapturePattern("captured", AnalyzerTestPrelude.intType))
    }
  }

  test("CapturePattern should fail when source type cannot be inferred") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSingleErrorContains(result, "Could not infer type")
  }

  test("CapturePattern should fail if the captured name is already defined in the environment") {
    val envWithCaptured = testEnv.copy(values = Namespace(("captured", AnalyzerTestPrelude.intType)))
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer(envWithCaptured, Env.empty).walkAssignment(pattern, Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "Cannot shadow existing value with same name `captured`")
  }

  test("PatternAs should apply target type to nested pattern") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, None)

    assertAnSuccess(result, (testEnv, Terms.WildcardPattern(AnalyzerTestPrelude.intType)))
  }

  test("PatternAs should fail when source type is incompatible with target type") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer(testEnv, Env.empty).walkAssignment(pattern, Some(Types.Uni))

    assertNotAssignableError(result)
  }
}
