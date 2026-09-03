package clara.analyzer.impl

import clara.ast.NoPos
import clara.asg.Types
import clara.testutil.BaseSpec

class ValueExprAnalyzerSpec extends BaseSpec {

  test("named value records the resolved definition usage") {
    val env = Env.empty.addOrShadowValue("value", Types.Uni, NoPos).value.value
    val envValue = env.getValue("value").get

    val result = ValueExprAnalyzer.namedValue(env, "value", NoPos)

    assert(result.value.value.typ === Types.Uni)
    assert(result.log.usageTrace.valueDefs === Map(envValue.uniq -> Set(NoPos)))
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
