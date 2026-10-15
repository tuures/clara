package clara.analyzer.impl

import clara.ast.{Ast, AstLiteral, NoPos}
import clara.asg.{Terms, Types}
import clara.testutil.{AnalyzerTestPrelude, BaseSpec}

class ValueExprAnalyzerSpec extends BaseSpec {

  test("named value records the resolved definition usage") {
    val env = Env.empty.addOrShadowValue("value", Types.Uni, NoPos).value.value
    val envValue = env.getValue("value").get

    val result = ValueExprAnalyzer.namedValue(env, "value", NoPos)

    assert(result.value.value.typ === Types.Uni)
    assert(result.log.usageTrace.valueDefs === Map(envValue.uniq -> Set(NoPos)))
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

  test("x @ #(1 => (), \"a\" => ()) accepts refutable cases matching parts of x's type") {
    val piecewise = Ast.Piecewise(Seq(
      (Ast.IntegerPattern(AstLiteral.IntegerDec("1")), Ast.UnitLiteral()),
      (Ast.StringPattern(Seq(AstLiteral.StringPlainPart("a"))), Ast.UnitLiteral()),
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
