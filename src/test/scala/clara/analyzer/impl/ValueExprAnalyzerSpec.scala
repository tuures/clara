package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.asg.{TypeCons, Types}
import clara.testutil.BaseSpec

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
    val aParamCon = TypeCons.ParamCon("A", NoPos)
    val aParam = Types.Param(aParamCon)
    val polymorphicFunction = Types.Func(
      Seq(aParamCon),
      Types.Tuple(Seq(Types.Func(aParam, Types.Uni), aParam)),
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





  // piecewise function with one piece should not produce intersection type
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
