package clara.analyzer.impl

import clara.testutil.BaseSpec

class ValueExprAnalyzerSpec extends BaseSpec {

  test("???") {
    ???
  }

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
