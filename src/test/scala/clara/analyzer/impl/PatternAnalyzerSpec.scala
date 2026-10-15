package clara.analyzer.impl

import clara.ast.{Ast, AstLiteral, NoPos, SourceInfo, SourcePos}
import clara.asg.{Namespace, TermLiteral, Terms, TypeCons, Types, Uniq}
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
    val result = PatternAnalyzer.walkAssignment(Env.empty, Ast.WildcardPattern(), None)

    assertAnSuccess(result, (Env.empty, Terms.WildcardPattern(Types.Top)))
  }

  test("WildcardPattern with known source type should keep that type") {
    val result = PatternAnalyzer.walkAssignment(Env.empty, Ast.WildcardPattern(), Some(Types.Uni))

    assertAnSuccess(result, (Env.empty, Terms.WildcardPattern(Types.Uni)))
  }

  test("UnitPattern should succeed with unknown source type") {
    val result = PatternAnalyzer.walkAssignment(testEnv, Ast.UnitPattern(), None)

    assertAnSuccess(result, (testEnv, Terms.UnitPattern()))
  }

  test("UnitPattern should fail when source type is incompatible") {
    val result = PatternAnalyzer.walkAssignment(testEnv, Ast.UnitPattern(), Some(AnalyzerTestPrelude.intType))

    assertNotAssignableError(result)
  }

  test("IntegerPattern should resolve Int and return typed pattern") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None)

    assertAnSuccess(result, (testEnv, Terms.IntegerPattern(TermLiteral.IntegerDec("42"), AnalyzerTestPrelude.intType)))
  }

  test("IntegerPattern should fail when source type is incompatible") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("IntegerPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("FloatPattern should resolve Float and return typed pattern") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None)

    assertAnSuccess(result, (testEnv, Terms.FloatPattern(TermLiteral.Float("3", "14"), AnalyzerTestPrelude.floatType)))
  }

  test("FloatPattern should fail when source type is incompatible") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("FloatPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.floatType))

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("StringPattern should resolve String and return typed pattern") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None)

    assertAnSuccess(result, (testEnv, Terms.StringPattern(Seq(TermLiteral.StringPlainPart("hello")), AnalyzerTestPrelude.stringType)))
  }

  test("StringPattern should fail when source type is incompatible") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("StringPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("StringPattern with a single capture matches every string so it can be used in an assignment") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringCapturePart(Ast.CapturePattern("s"))))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    inside(result.value) {
      case Right((nextEnv, _)) => assert(nextEnv.getValue("s").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
    }
  }

  test("StringPattern should handle escape sequences, captures and named constants") {
    val envWithConstant = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("ConstStr", EnvValue(AnalyzerTestPrelude.stringType, NoPos, Uniq()))
    )))
    val pattern = Ast.StringPattern(Seq(
      AstLiteral.StringPlainPart("hello "),
      AstLiteral.StringCapturePart(Ast.CapturePattern("captured")),
      AstLiteral.StringNamedConstantPart(Ast.NamedConstantPattern("ConstStr")),
      AstLiteral.StringEscapePart(Seq("n"))
    ))

    val result = PatternAnalyzer.walkCase(envWithConstant, pattern, None)

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(term.isInstanceOf[Terms.StringPattern])
        assert(nextEnv.getValue("captured").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
    }
  }

  test("TuplePattern should succeed with unknown source type") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.WildcardPattern()))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, None)

    val expectedTerm = Terms.TuplePattern(
      Seq(Terms.UnitPattern(), Terms.WildcardPattern(Types.Top)),
      Types.Tuple(Seq(Types.Uni, Types.Top))
    )
    assertAnSuccess(result, (testEnv, expectedTerm))
  }

  test("TuplePattern should fail when source type is not a tuple") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.WildcardPattern()))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    assertNotAssignableError(result)
  }

  test("TuplePattern should fail when source tuple has incompatible element types") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.UnitPattern()))
    val fromType = Types.Tuple(Seq(Types.Uni, AnalyzerTestPrelude.intType))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(fromType))

    assertNotAssignableError(result)
  }

  test("NamedConstantPattern should resolve named value from environment") {
    val envWithValue = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("Const1", EnvValue(AnalyzerTestPrelude.intType, NoPos, Uniq()))
    )))
    val pattern = Ast.NamedConstantPattern("Const1")

    val result = PatternAnalyzer.walkAssignment(envWithValue, pattern, None)

    assertAnSuccess(result, (
      envWithValue,
      Terms.NamedConstantPattern(Terms.NamedValue("Const1", AnalyzerTestPrelude.intType))
    ))
  }

  test("NamedConstantPattern should fail for unknown value") {
    val pattern = Ast.NamedConstantPattern("MissingValue")

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, None)

    assertAnSingleErrorContains(result, "Unknown value")
  }

  test("CapturePattern should bind captured name with inferred source type") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(nextEnv.getValue("captured").map(_.typ) === Some(AnalyzerTestPrelude.intType))
        assert(term === Terms.CapturePattern("captured", AnalyzerTestPrelude.intType))
    }
  }

  test("CapturePattern should fail when source type cannot be inferred") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, None)

    assertAnSingleErrorContains(result, "Could not infer type")
  }

  test("CapturePattern should fail if the captured name is already defined in the environment") {
    val envWithCaptured = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("captured", EnvValue(AnalyzerTestPrelude.intType, NoPos, Uniq()))
    )))
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkAssignment(envWithCaptured, pattern, Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "Cannot shadow existing value with same name `captured`")
  }

  test("PatternAs should apply target type to nested pattern") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, None)

    assertAnSuccess(result, (testEnv, Terms.PatternAs(Terms.WildcardPattern(AnalyzerTestPrelude.intType), AnalyzerTestPrelude.intType)))
  }

  test("PatternAs should fail when source type is incompatible with target type") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(Types.Uni))

    assertNotAssignableError(result)
  }

  // refutable patterns (cases of #()) only need to be able to match some value of the source type

  private val intOrString = Types.Union(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType))
  private val trueType = Types.Singleton(TypeCons.SingletonTypeCon("True", NoPos))
  private val falseType = Types.Singleton(TypeCons.SingletonTypeCon("False", NoPos))
  private val booleanEnv = testEnv.copy(local = testEnv.local.copy(values = Namespace(
    ("True", EnvValue(trueType, NoPos, Uniq())),
    ("False", EnvValue(falseType, NoPos, Uniq())),
  )))

  test("refutable IntegerPattern should narrow a union source type to Int") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("1"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(intOrString))

    assertAnSuccess(result, (testEnv, Terms.IntegerPattern(TermLiteral.IntegerDec("1"), AnalyzerTestPrelude.intType)))
  }

  test("refutable IntegerPattern should fail when it can never match the source type") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("1"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    assertAnSingleErrorContains(result, "Pattern of type `Int` can never match a value of type `String`")
  }

  test("refutable NamedConstantPattern should match a member of a union of singletons") {
    val result = PatternAnalyzer.walkCase(booleanEnv, Ast.NamedConstantPattern("True"), Some(Types.Union(Seq(trueType, falseType))))

    assertAnSuccess(result, (booleanEnv, Terms.NamedConstantPattern(Terms.NamedValue("True", trueType))))
  }

  test("refutable NamedConstantPattern should fail when it can never match the source type") {
    val result = PatternAnalyzer.walkCase(booleanEnv, Ast.NamedConstantPattern("True"), Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("refutable TuplePattern should narrow to the tuples of a union source type") {
    val pattern = Ast.TuplePattern(Seq(Ast.IntegerPattern(AstLiteral.IntegerDec("1")), Ast.CapturePattern("x")))
    val fromType = Types.Union(Seq(
      Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType)),
      AnalyzerTestPrelude.intType,
    ))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(fromType))

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(nextEnv.getValue("x").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
        assert(term.typ === Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType)))
    }
  }

  test("refutable TuplePattern should fail when the source type has no tuples") {
    val pattern = Ast.TuplePattern(Seq(Ast.CapturePattern("x"), Ast.CapturePattern("y")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("irrefutable TuplePattern should destructure a union of tuples") {
    val pattern = Ast.TuplePattern(Seq(Ast.CapturePattern("x"), Ast.CapturePattern("y")))
    val fromType = Types.Union(Seq(
      Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.intType)),
      Types.Tuple(Seq(AnalyzerTestPrelude.stringType, AnalyzerTestPrelude.intType)),
    ))

    val result = PatternAnalyzer.walkAssignment(testEnv, pattern, Some(fromType))

    inside(result.value) {
      case Right((nextEnv, _)) =>
        assert(nextEnv.getValue("x").map(_.typ) === Some(intOrString))
        assert(nextEnv.getValue("y").map(_.typ) === Some(AnalyzerTestPrelude.intType))
    }
  }

  test("refutable PatternAs should narrow the nested pattern to the target type") {
    val pattern = Ast.PatternAs(Ast.CapturePattern("n"), Ast.NamedType(Ast.NameWithPos("Int"), Nil))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(intOrString))

    inside(result.value) {
      case Right((nextEnv, _)) =>
        assert(nextEnv.getValue("n").map(_.typ) === Some(AnalyzerTestPrelude.intType))
    }
  }

  test("refutable PatternAs should fail when the target type can never match") {
    val pattern = Ast.PatternAs(Ast.CapturePattern("n"), Ast.NamedType(Ast.NameWithPos("Int"), Nil))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType))

    assertAnSingleErrorContains(result, "can never match")
  }

  test("refutable pattern that can never match quotes the pattern's source in the error") {
    val source = "\"a\""
    val pos = SourcePos(SourceInfo.fromString("test", source), 0, Some(source.length))
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("a")), pos)

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType))

    //TODO: Instead of embedding source quote to the message, maybe have error printer handle it based on the Pos?
    assertAnSingleErrorContains(result, "Pattern `\"a\"` of type `String` can never match a value of type `Int`")
  }
}
