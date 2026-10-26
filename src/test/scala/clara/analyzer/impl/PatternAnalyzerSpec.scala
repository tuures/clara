package clara.analyzer.impl

import clara.ast.{Ast, AstLiteral, NoPos, SourceInfo, SourcePos}
import clara.asg.{DefId, Namespace, TermLiteral, Terms, TypeCons, Types}
import clara.testutil.{AnalyzerTestPrelude, BaseSpec}

class PatternAnalyzerSpec extends BaseSpec {
  private val testEnv = AnalyzerTestPrelude.env

  // TODO: move these to some test utility object since they could be used in multiple places
  private def assertAnValid[A](result: AnResult[A], expected: A): Unit = {
    val _ = assert(result.value.value === expected)
  }

  private def assertNotAssignableError(result: AnResult[_]): Unit = {
    assertAnSingleErrorContains(result, "is not assignable")
  }

  private def assertAnSingleErrorContains(result: AnResult[_], expectedText: String): Unit = {
    val _ = inside(result.value) {
      case Left(Seq(error)) =>
        assert(error.message.contains(expectedText))
    }
  }

  test("WildcardPattern with unknown source type should use Top") {
    val result = PatternAnalyzer.walkParameter(Env.empty, Ast.WildcardPattern(), None).run

    assertAnValid(result, (Env.empty, Terms.WildcardPattern(Types.Top)))
  }

  test("WildcardPattern with known source type should keep that type") {
    val result = PatternAnalyzer.walkParameter(Env.empty, Ast.WildcardPattern(), Some(Types.Uni)).run

    assertAnValid(result, (Env.empty, Terms.WildcardPattern(Types.Uni)))
  }

  test("UnitPattern should succeed with unknown source type") {
    val result = PatternAnalyzer.walkParameter(testEnv, Ast.UnitPattern(), None).run

    assertAnValid(result, (testEnv, Terms.UnitPattern()))
  }

  test("UnitPattern should fail when source type is incompatible") {
    val result = PatternAnalyzer.walkParameter(testEnv, Ast.UnitPattern(), Some(AnalyzerTestPrelude.intType)).run

    assertNotAssignableError(result)
  }

  test("IntegerPattern should resolve Int and return typed pattern") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None).run

    assertAnValid(result, (testEnv, Terms.IntegerPattern(TermLiteral.IntegerDec("42"), AnalyzerTestPrelude.intType)))
  }

  test("IntegerPattern should fail when source type is incompatible") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("IntegerPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("42"))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("FloatPattern should resolve Float and return typed pattern") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None).run

    assertAnValid(result, (testEnv, Terms.FloatPattern(TermLiteral.Float("3", "14"), AnalyzerTestPrelude.floatType)))
  }

  test("FloatPattern should fail when source type is incompatible") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("FloatPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.FloatPattern(AstLiteral.Float("3", "14"))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.floatType)).run

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("StringPattern should resolve String and return typed pattern") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, None).run

    assertAnValid(result, (testEnv, Terms.StringPattern(Seq(TermLiteral.StringPlainPart("hello")), AnalyzerTestPrelude.stringType)))
  }

  test("StringPattern should fail when source type is incompatible") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("StringPattern should fail in an assignment since it can fail to match") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("hello")))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    assertAnSingleErrorContains(result, "can fail to match")
  }

  test("StringPattern with a single capture matches every string so it can be used in an assignment") {
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringCapturePart(Ast.CapturePattern("s"))))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    inside(result.value) {
      case Right((nextEnv, _)) => assert(nextEnv.getValue("s").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
    }
  }

  test("StringPattern should handle escape sequences, captures and named constants") {
    val envWithConstant = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("ConstStr", EnvValue(AnalyzerTestPrelude.stringType, DefId(), Nil, NoPos))
    )))
    val pattern = Ast.StringPattern(Seq(
      AstLiteral.StringPlainPart("hello "),
      AstLiteral.StringCapturePart(Ast.CapturePattern("captured")),
      AstLiteral.StringNamedConstantPart(Ast.NamedConstantPattern(Nil, Ast.NameWithPos("ConstStr"))),
      AstLiteral.StringEscapePart(Seq("n"))
    ))

    val result = PatternAnalyzer.walkCase(envWithConstant, pattern, None).run

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(term.isInstanceOf[Terms.StringPattern])
        assert(nextEnv.getValue("captured").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
    }
  }

  test("TuplePattern should succeed with unknown source type") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.WildcardPattern()))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, None).run

    val expectedTerm = Terms.TuplePattern(
      Seq(Terms.UnitPattern(), Terms.WildcardPattern(Types.Top)),
      Types.Tuple(Seq(Types.Uni, Types.Top))
    )
    assertAnValid(result, (testEnv, expectedTerm))
  }

  test("TuplePattern should fail when source type is not a tuple") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.WildcardPattern()))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    assertNotAssignableError(result)
  }

  test("TuplePattern should fail when source tuple has incompatible element types") {
    val pattern = Ast.TuplePattern(Seq(Ast.UnitPattern(), Ast.UnitPattern()))
    val fromType = Types.Tuple(Seq(Types.Uni, AnalyzerTestPrelude.intType))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(fromType)).run

    assertNotAssignableError(result)
  }

  test("NamedConstantPattern should resolve named value from environment") {
    val envWithValue = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("Const1", EnvValue(AnalyzerTestPrelude.intType, DefId(), Nil, NoPos))
    )))
    val pattern = Ast.NamedConstantPattern(Nil, Ast.NameWithPos("Const1"))

    val result = PatternAnalyzer.walkParameter(envWithValue, pattern, None).run

    assertAnValid(result, (
      envWithValue,
      Terms.NamedConstantPattern(Terms.NamedValue("Const1", Nil, AnalyzerTestPrelude.intType))
    ))
  }

  test("NamedConstantPattern should fail for unknown value") {
    val pattern = Ast.NamedConstantPattern(Nil, Ast.NameWithPos("MissingValue"))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, None).run

    assertAnSingleErrorContains(result, "Unknown value")
  }

  test("CapturePattern should bind captured name with inferred source type") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(nextEnv.getValue("captured").map(_.typ) === Some(AnalyzerTestPrelude.intType))
        assert(term === Terms.CapturePattern("captured", AnalyzerTestPrelude.intType))
    }
  }

  test("CapturePattern should fail when source type cannot be inferred") {
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, None).run

    assertAnSingleErrorContains(result, "Could not infer type")
  }

  test("CapturePattern should fail if the captured name is already defined in the environment") {
    val envWithCaptured = testEnv.copy(local = testEnv.local.copy(values = Namespace(
      ("captured", EnvValue(AnalyzerTestPrelude.intType, DefId(), Nil, NoPos))
    )))
    val pattern = Ast.CapturePattern("captured")

    val result = PatternAnalyzer.walkParameter(envWithCaptured, pattern, Some(AnalyzerTestPrelude.intType)).run

    assertAnSingleErrorContains(result, "Cannot shadow existing value with same name `captured`")
  }

  test("PatternAs should apply target type to nested pattern") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Nil, Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, None).run

    assertAnValid(result, (testEnv, Terms.PatternAs(Terms.WildcardPattern(AnalyzerTestPrelude.intType), AnalyzerTestPrelude.intType)))
  }

  test("PatternAs should fail when source type is incompatible with target type") {
    val pattern = Ast.PatternAs(
      Ast.WildcardPattern(),
      Ast.NamedType(Nil, Ast.NameWithPos("Int"), Nil)
    )

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(Types.Uni)).run

    assertNotAssignableError(result)
  }

  // refutable patterns (cases of #()) only need to be able to match some value of the source type

  private val intOrString = Types.Union(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType))
  private val trueType = Types.Singleton(TypeCons.SingletonTypeCon("True", Nil, NoPos))
  private val falseType = Types.Singleton(TypeCons.SingletonTypeCon("False", Nil, NoPos))
  private val booleanEnv = testEnv.copy(local = testEnv.local.copy(values = Namespace(
    ("True", EnvValue(trueType, DefId(), Nil, NoPos)),
    ("False", EnvValue(falseType, DefId(), Nil, NoPos)),
  )))

  test("refutable IntegerPattern should narrow a union source type to Int") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("1"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(intOrString)).run

    assertAnValid(result, (testEnv, Terms.IntegerPattern(TermLiteral.IntegerDec("1"), AnalyzerTestPrelude.intType)))
  }

  test("refutable IntegerPattern should fail when it can never match the source type") {
    val pattern = Ast.IntegerPattern(AstLiteral.IntegerDec("1"))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    assertAnSingleErrorContains(result, "Pattern of type `Int` can never match a value of type `String`")
  }

  test("refutable NamedConstantPattern should match a member of a union of singletons") {
    val result = PatternAnalyzer.walkCase(booleanEnv, Ast.NamedConstantPattern(Nil, Ast.NameWithPos("True")), Some(Types.Union(Seq(trueType, falseType)))).run

    assertAnValid(result, (booleanEnv, Terms.NamedConstantPattern(Terms.NamedValue("True", Nil, trueType))))
  }

  test("refutable NamedConstantPattern should fail when it can never match the source type") {
    val result = PatternAnalyzer.walkCase(booleanEnv, Ast.NamedConstantPattern(Nil, Ast.NameWithPos("True")), Some(AnalyzerTestPrelude.intType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("refutable TuplePattern should narrow to the tuples of a union source type") {
    val pattern = Ast.TuplePattern(Seq(Ast.IntegerPattern(AstLiteral.IntegerDec("1")), Ast.CapturePattern("x")))
    val fromType = Types.Union(Seq(
      Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType)),
      AnalyzerTestPrelude.intType,
    ))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(fromType)).run

    inside(result.value) {
      case Right((nextEnv, term)) =>
        assert(nextEnv.getValue("x").map(_.typ) === Some(AnalyzerTestPrelude.stringType))
        assert(term.typ === Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.stringType)))
    }
  }

  test("refutable TuplePattern should fail when the source type has no tuples") {
    val pattern = Ast.TuplePattern(Seq(Ast.CapturePattern("x"), Ast.CapturePattern("y")))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("irrefutable TuplePattern should destructure a union of tuples") {
    val pattern = Ast.TuplePattern(Seq(Ast.CapturePattern("x"), Ast.CapturePattern("y")))
    val fromType = Types.Union(Seq(
      Types.Tuple(Seq(AnalyzerTestPrelude.intType, AnalyzerTestPrelude.intType)),
      Types.Tuple(Seq(AnalyzerTestPrelude.stringType, AnalyzerTestPrelude.intType)),
    ))

    val result = PatternAnalyzer.walkParameter(testEnv, pattern, Some(fromType)).run

    inside(result.value) {
      case Right((nextEnv, _)) =>
        assert(nextEnv.getValue("x").map(_.typ) === Some(intOrString))
        assert(nextEnv.getValue("y").map(_.typ) === Some(AnalyzerTestPrelude.intType))
    }
  }

  test("refutable PatternAs should narrow the nested pattern to the target type") {
    val pattern = Ast.PatternAs(Ast.CapturePattern("n"), Ast.NamedType(Nil, Ast.NameWithPos("Int"), Nil))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(intOrString)).run

    inside(result.value) {
      case Right((nextEnv, _)) =>
        assert(nextEnv.getValue("n").map(_.typ) === Some(AnalyzerTestPrelude.intType))
    }
  }

  test("refutable PatternAs should fail when the target type can never match") {
    val pattern = Ast.PatternAs(Ast.CapturePattern("n"), Ast.NamedType(Nil, Ast.NameWithPos("Int"), Nil))

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.stringType)).run

    assertAnSingleErrorContains(result, "can never match")
  }

  test("refutable pattern that can never match quotes the pattern's source in the error") {
    val source = "\"a\""
    val pos = SourcePos(SourceInfo.fromString("test", source), 0, Some(source.length))
    val pattern = Ast.StringPattern(Seq(AstLiteral.StringPlainPart("a")), pos)

    val result = PatternAnalyzer.walkCase(testEnv, pattern, Some(AnalyzerTestPrelude.intType)).run

    //TODO: Instead of embedding source quote to the message, maybe have error printer handle it based on the Pos?
    assertAnSingleErrorContains(result, "Pattern `\"a\"` of type `String` can never match a value of type `Int`")
  }

  // the names a pattern binds, declared by the names pass of a scope

  test("walkValueDef defines the names the names pass declared, keeping their DefId") {
    val env = Env.empty.declareValue("x", NoPos).run.value.value
    val declaredDefId = env.local.values.get("x").map(_.defId)

    val result = PatternAnalyzer.walkValueDef(env, Ast.CapturePattern("x"), Some(Types.Uni)).run

    inside(result.value.value) { case (nextEnv, _) =>
      assert(nextEnv.local.values.get("x") === declaredDefId.map(EnvValue(Types.Uni, _, Nil, NoPos)))
    }
  }

  test("boundNames are the captures of a pattern, nested ones included, in source order") {
    val pattern = Ast.TuplePattern(Seq(
      Ast.CapturePattern("a"),
      Ast.PatternAs(Ast.CapturePattern("b"), Ast.UnitType()),
      Ast.StringPattern(Seq(AstLiteral.StringPlainPart("x"), AstLiteral.StringCapturePart(Ast.CapturePattern("c")))),
      Ast.WildcardPattern(),
      Ast.NamedConstantPattern(Nil, Ast.NameWithPos("Constant")),
    ))

    assert(PatternAnalyzer.boundNames(pattern).map(_.name) === Seq("a", "b", "c"))
  }
}
