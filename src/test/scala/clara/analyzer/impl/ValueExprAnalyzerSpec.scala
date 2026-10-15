package clara.analyzer.impl

import clara.ast.{Ast, AstLiteral, NoPos}
import clara.asg.{Terms, TypeCons, Types}
import clara.testutil.{AnalyzerTestPrelude, BaseSpec}

class ValueExprAnalyzerSpec extends BaseSpec {

  test("named value records the resolved definition usage") {
    val env = Env.empty.addOrShadowValue("value", Types.Uni, NoPos).value.value
    val envValue = env.getValue("value").get

    val result = ValueExprAnalyzer.namedValue(env, "value", NoPos)

    assert(result.value.value.typ === Types.Uni)
    assert(result.log.usageTrace.defs === Map(envValue.defId -> Set(NoPos)))
  }

  // FIXME
  // test("polymorphic call unifies repeated record constraints to their common supertype") {
  //   val aParamCon = TypeCons.ParamCon("A", NoPos)
  //   val aParam = Types.Param(aParamCon)
  //   val polymorphicFunction = Types.Func(
  //     Seq(aParamCon),
  //     Types.Tuple(Seq(
  //       Types.Record("value" -> aParam),
  //       Types.Record("value" -> aParam),
  //     )),
  //     Types.Uni,
  //   )
  //   val argument = Types.Tuple(Seq(
  //     Types.Record("value" -> Types.Uni),
  //     Types.Record("value" -> Types.Top),
  //   ))
  //   val env = Env.empty.
  //     addOrShadowValue("f", polymorphicFunction, NoPos).value.value.
  //     addOrShadowValue("argument", argument, NoPos).value.value

  //   val result = ValueExprAnalyzer.valueExprTerm(
  //     env,
  //     Ast.Call(Ast.NamedValue("f"), Ast.NamedValue("argument")),
  //   )

  //   assert(result.value.isRight)
  // }

 test("polymorphic call fails when a parameter has incompatible covariant and contravariant constraints") {
    val polymorphicFunction = Types.Func(
      Seq(AnalyzerTestPrelude.aParamCon),
      Types.Tuple(Seq(Types.Func(AnalyzerTestPrelude.aParam, Types.Uni), AnalyzerTestPrelude.aParam)),
      Types.Uni,
    )
    val argument = Types.Tuple(Seq(Types.Func(Types.Bottom, Types.Uni), Types.Uni))
    val env = Env.empty.
      addOrShadowValue("f", polymorphicFunction, NoPos).value.value.
      addOrShadowValue("argument", argument, NoPos).value.value

    val result = ValueExprAnalyzer.valueExprTerm(
      env,
      Ast.Call(Ast.NamedValue("f"), Ast.NamedValue("argument")),
    )

    assert(result.value.isLeft)
    assert(result.value.left.value.length === 1)
    // FIXME the error message is not very good, it should be more specific about the type parameter and the constraints that are incompatible
    assert(result.value.left.value.head.message === "Type `(! => (), ())` is not assignable to type `(! => (), !)`")
  }





  // the matched value's type flows into literal #() and lambda callees and into #() passed as arguments

  private val intType = AnalyzerTestPrelude.intType
  private val stringType = AnalyzerTestPrelude.stringType
  private val intOrString = Types.Union(Seq(intType, stringType))
  private def envWith(name: String, typ: Types.Type) =
    AnalyzerTestPrelude.env.addOrShadowValue(name, typ, NoPos).value.value

  test("x @ #(n => n) infers the capture's type from x") {
    val piecewise = Ast.Piecewise(Seq((Ast.CapturePattern("n"), Ast.NamedValue("n"))))

    val result = ValueExprAnalyzer.valueExprTerm(envWith("x", intOrString), Ast.Pipe(Ast.NamedValue("x"), piecewise))

    assert(result.value.value.typ === intOrString)
  }

  test("x @ #(1 => (), \"a\" => (), _ => ()) accepts refutable cases matching parts of x's type") {
    val piecewise = Ast.Piecewise(Seq(
      (Ast.IntegerPattern(AstLiteral.IntegerDec("1")), Ast.UnitLiteral()),
      (Ast.StringPattern(Seq(AstLiteral.StringPlainPart("a"))), Ast.UnitLiteral()),
      (Ast.WildcardPattern(), Ast.UnitLiteral()),
    ))

    val result = ValueExprAnalyzer.valueExprTerm(envWith("x", intOrString), Ast.Pipe(Ast.NamedValue("x"), piecewise))

    assert(Types.isAssignable(result.value.value.typ, Types.Uni))
  }

  test("x @ #(\"a\" => ()) fails when a case can never match x") {
    val piecewise = Ast.Piecewise(Seq((Ast.StringPattern(Seq(AstLiteral.StringPlainPart("a"))), Ast.UnitLiteral())))

    val result = ValueExprAnalyzer.valueExprTerm(envWith("x", intType), Ast.Pipe(Ast.NamedValue("x"), piecewise))

    assert(result.value.left.value.map(_.message) === Seq("Pattern of type `String` can never match a value of type `Int`"))
  }

  test("x @ (n => n) infers the lambda parameter's type from x") {
    val lambda = Ast.Lambda(Nil, Ast.CapturePattern("n"), Ast.NamedValue("n"))

    val result = ValueExprAnalyzer.valueExprTerm(envWith("x", intType), Ast.Pipe(Ast.NamedValue("x"), lambda))

    assert(result.value.value.typ === intType)
  }

  test("#(n => ()) passed to a function infers the capture's type from the function's parameter") {
    val piecewise = Ast.Piecewise(Seq((Ast.CapturePattern("n"), Ast.UnitLiteral())))
    val env = envWith("f", Types.Func(Types.Func(intType, Types.Uni), Types.Uni))

    val result = ValueExprAnalyzer.valueExprTerm(env, Ast.Call(Ast.NamedValue("f"), piecewise))

    assert(result.value.value.typ === Types.Uni)
  }

  test("f((n => n, 0)) infers the parameter type of a lambda nested in a tuple argument") {
    val argument = Ast.Tuple(Seq(Ast.Lambda(Nil, Ast.CapturePattern("n"), Ast.NamedValue("n")), Ast.IntegerLiteral(AstLiteral.IntegerDec("0"))))
    val env = envWith("f", Types.Func(Types.Tuple(Seq(Types.Func(intType, intType), intType)), Types.Uni))

    val result = ValueExprAnalyzer.valueExprTerm(env, Ast.Call(Ast.NamedValue("f"), argument))

    assert(result.value.value.typ === Types.Uni)
  }

  test("f({g = #(n => ())}) infers the capture type of a piecewise nested in a record argument") {
    val argument = Ast.Record(Seq(Ast.FieldDef("g", None, Ast.Piecewise(Seq((Ast.CapturePattern("n"), Ast.UnitLiteral()))))))
    val env = envWith("f", Types.Func(Types.Record("g" -> Types.Func(intType, Types.Uni)), Types.Uni))

    val result = ValueExprAnalyzer.valueExprTerm(env, Ast.Call(Ast.NamedValue("f"), argument))

    assert(result.value.value.typ === Types.Uni)
  }

  test("{g: Int => Int = n => n} infers the lambda's parameter type from the field's type annotation") {
    val intTypeExpr = Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    val record = Ast.Record(Seq(Ast.FieldDef(
      "g",
      Some(Ast.FuncType(Nil, intTypeExpr, intTypeExpr)),
      Ast.Lambda(Nil, Ast.CapturePattern("n"), Ast.NamedValue("n")),
    )))

    val result = ValueExprAnalyzer.valueExprTerm(AnalyzerTestPrelude.env, record)

    assert(result.value.value.typ === Types.Record("g" -> Types.Func(intType, intType)))
  }

  test("x: Int | String declares the wider type") {
    val intOrStringExpr = Ast.UnionType(Seq(Ast.NamedType(Ast.NameWithPos("Int"), Nil), Ast.NamedType(Ast.NameWithPos("String"), Nil)))

    val result = ValueExprAnalyzer.valueExprTerm(envWith("x", intType), Ast.ValueAs(Ast.NamedValue("x"), intOrStringExpr))

    assert(result.value.value === Terms.ValueAs(Terms.NamedValue("x", intType), intOrString))
  }

  test("x: String fails when x's type is not assignable to the declared type") {
    val result = ValueExprAnalyzer.valueExprTerm(
      envWith("x", intType),
      Ast.ValueAs(Ast.NamedValue("x"), Ast.NamedType(Ast.NameWithPos("String"), Nil)),
    )

    assert(result.value.left.value.map(_.message) === Seq("Type `Int` is not assignable to type `String`"))
  }

  test("(n => n): Int => Int infers the lambda's parameter type from the declared type") {
    val intTypeExpr = Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    val valueAs = Ast.ValueAs(Ast.Lambda(Nil, Ast.CapturePattern("n"), Ast.NamedValue("n")), Ast.FuncType(Nil, intTypeExpr, intTypeExpr))

    val result = ValueExprAnalyzer.valueExprTerm(AnalyzerTestPrelude.env, valueAs)

    assert(result.value.value.typ === Types.Func(intType, intType))
  }

  // exhaustiveness and reachability of the cases of #()

  private val trueType = Types.Singleton(TypeCons.SingletonTypeCon("True", NoPos))
  private val falseType = Types.Singleton(TypeCons.SingletonTypeCon("False", NoPos))
  private val booleanType = Types.Union(Seq(trueType, falseType))
  private def booleanEnvWith(name: String, typ: Types.Type) = envWith(name, typ).
    addOrShadowValue("True", trueType, NoPos).value.value.
    addOrShadowValue("False", falseType, NoPos).value.value
  private def unitCase(pattern: Ast.Pattern) = (pattern, Ast.UnitLiteral())
  private def int(value: String) = Ast.IntegerPattern(AstLiteral.IntegerDec(value))
  private def matchX(env: Env, cases: (Ast.Pattern, Ast.ValueExpr)*) =
    ValueExprAnalyzer.valueExprTerm(env, Ast.Pipe(Ast.NamedValue("x"), Ast.Piecewise(cases)))

  test("cases matching every singleton of a union are exhaustive") {
    val result = matchX(booleanEnvWith("x", booleanType),
      unitCase(Ast.NamedConstantPattern("True")), unitCase(Ast.NamedConstantPattern("False")))

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
  }

  test("cases missing a singleton of a union are not exhaustive") {
    val result = matchX(booleanEnvWith("x", booleanType), unitCase(Ast.NamedConstantPattern("True")))

    assert(result.value.left.value.map(_.message) === Seq(
      "Cases of the piecewise function don't match every value of type `True | False`, e.g. add a last case `_ => ...`"
    ))
  }

  test("literal cases never cover their whole type") {
    val result = matchX(envWith("x", intType), unitCase(int("1")), unitCase(int("2")))

    assert(result.value.left.value.map(_.message).head.startsWith("Cases of the piecewise function don't match every value of type `Int`"))
  }

  test("a capture after literal cases makes the cases exhaustive") {
    val result = matchX(envWith("x", intType), unitCase(int("1")), unitCase(Ast.CapturePattern("n")))

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
  }

  test("a case after a capture is unreachable") {
    val result = matchX(envWith("x", intType), unitCase(Ast.CapturePattern("n")), unitCase(int("1")))

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Pattern is unreachable, earlier cases already match all its values"))
  }

  test("a literal case after the same literal is still reachable since literals cover nothing") {
    val result = matchX(envWith("x", intType), unitCase(int("1")), unitCase(int("1")), unitCase(Ast.WildcardPattern()))

    // FIXME without literal types the duplicate literal can't be detected as unreachable
    assert(result.log.warnings.isEmpty)
  }

  test("tuple cases are exhaustive when together they cover every combination") {
    val result = matchX(booleanEnvWith("x", Types.Tuple(Seq(booleanType, intType))),
      unitCase(Ast.TuplePattern(Seq(Ast.NamedConstantPattern("True"), Ast.CapturePattern("n")))),
      unitCase(Ast.TuplePattern(Seq(Ast.NamedConstantPattern("False"), Ast.CapturePattern("n")))),
    )

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
  }

  test("a tuple case with a literal element covers nothing") {
    val result = matchX(booleanEnvWith("x", Types.Tuple(Seq(booleanType, intType))),
      unitCase(Ast.TuplePattern(Seq(Ast.NamedConstantPattern("True"), int("1")))),
      unitCase(Ast.TuplePattern(Seq(Ast.NamedConstantPattern("False"), Ast.CapturePattern("n")))),
    )

    assert(result.value.left.value.map(_.message).head.startsWith("Cases of the piecewise function don't match every value"))
  }

  test("type pattern cases that would need a runtime type test are rejected for now") {
    val intTypeExpr = Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    val stringTypeExpr = Ast.NamedType(Ast.NameWithPos("String"), Nil)
    val result = matchX(envWith("x", intOrString),
      unitCase(Ast.PatternAs(Ast.CapturePattern("n"), intTypeExpr)),
      unitCase(Ast.PatternAs(Ast.CapturePattern("s"), stringTypeExpr)),
    )

    assert(result.value.left.value.map(_.message) === Seq(
      "Pattern needs to test the type of the value at runtime, which isn't supported yet",
      "Pattern needs to test the type of the value at runtime, which isn't supported yet",
    ))
  }

  test("a type pattern covers its type when it needs no runtime type test, e.g. (n: Int, True) and (n: Int, False)") {
    val intTypeExpr = Ast.NamedType(Ast.NameWithPos("Int"), Nil)
    val result = matchX(booleanEnvWith("x", Types.Tuple(Seq(intType, booleanType))),
      unitCase(Ast.TuplePattern(Seq(Ast.PatternAs(Ast.CapturePattern("n"), intTypeExpr), Ast.NamedConstantPattern("True")))),
      unitCase(Ast.TuplePattern(Seq(Ast.PatternAs(Ast.CapturePattern("n"), intTypeExpr), Ast.NamedConstantPattern("False")))),
    )

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
  }

  test("a single capture string pattern on Int | String would need a runtime type test") {
    val result = matchX(envWith("x", intOrString),
      unitCase(Ast.StringPattern(Seq(AstLiteral.StringCapturePart(Ast.CapturePattern("s"))))),
      unitCase(Ast.WildcardPattern()),
    )

    assert(result.value.left.value.map(_.message) === Seq("Pattern needs to test the type of the value at runtime, which isn't supported yet"))
  }

  test("#(1 => ()) without an expected type is not exhaustive for the Int it can be called with") {
    val result = ValueExprAnalyzer.valueExprTerm(AnalyzerTestPrelude.env, Ast.Piecewise(Seq(unitCase(int("1")))))

    assert(result.value.left.value.map(_.message).head.startsWith("Cases of the piecewise function don't match every value of type `Int`"))
  }

  test("#() with one case is typed as a function, not an intersection") {
    val piecewise = Ast.Piecewise(Seq((Ast.UnitPattern(), Ast.UnitLiteral())))

    val result = ValueExprAnalyzer.valueExprTerm(AnalyzerTestPrelude.env, piecewise)

    assert(result.value.value.typ === Types.Func(Types.Uni, Types.Uni))
  }

  // empty piecewise function

  //   ve("Calling function yields result type", "()") {
//     Call(Lambda(UnitPattern(), UnitLiteral()), UnitLiteral())
//   }
//
//   ve("Calling a higher-order function should infer type of the lambda that has no type annotation in the parameter pattern", "Int") {
//     val strLength = Lambda(
//       NamePattern("str"), // no type annotation
//       MemberSelection(NamedValue("str"), NamedMember("length", Nil))
//     )
//
//     val applyToHello = Lambda(
//       // f: String => Int
//       PatternAs(NamePattern("f"), FuncType(NamedType("String", Nil), NamedType("Int", Nil))),
//       Call(NamedValue("f"), StringLiteral(Seq(LiteralValue.StringPlainPart("Hello"))))
//     )
//
//     Call(applyToHello, strLength)
//   }
//


//   ve("Selecting plain value member", "Int") {
//     MemberSelection(StringLiteral(Seq(LiteralValue.StringPlainPart("foo"))), NamedMember("length", Nil))
//   }


// TODO: add test for type arg inference of calling polymorphic function

}
