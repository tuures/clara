// package clara
//
// import org.scalatest._
//
// import ai.x.safe._
//
// class AnalyzerSpec extends FunSuite {
//   import Ast._
//
//   def ve(testName: String, expectedSignature: String)(valueExpr: ValueExpr) = test(testName) {
//     Analyzer.analyze(Prelude.prependTo(valueExpr)) match {
//       case Right(t) => assert(t.signature(Analyzer.Env.empty) == expectedSignature)
//       case Left(errors) => fail(errors.map(_.humanFormat).safeMkString("\n"))
//     }
//   }
//
//   def err(testName: String)(expectedErrorMessages: Seq[String])(valueExpr: ValueExpr) = test(testName) {
//     Analyzer.analyze(Prelude.prependTo(valueExpr)) match {
//       case Right(t) => fail("should not have been ok")
//       case Left(errors) => assert(errors.map(_.message) == expectedErrorMessages)
//     }
//   }
//
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
//       Call(NamedValue("f"), StringLiteral(Seq(StringLiteralPlainPart("Hello"))))
//     )
//
//     Call(applyToHello, strLength)
//   }
//
//   ve("Block yields type of the last expression", "String") {
//     Block(Seq(UnitLiteral(), StringLiteral(Seq(StringLiteralPlainPart("foo")))))
//   }
//
//   ve("Selecting plain value member", "Int") {
//     MemberSelection(StringLiteral(Seq(StringLiteralPlainPart("foo"))), NamedMember("length", Nil))
//   }
//
//   ve("Class definition and instantiation", "Foo[String]") {
//     Block(Seq(
//       ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
//         ValueDecl("bar", NamedType("String", Nil)),
//         ValueDecl("zot", NamedType("A", Nil))
//       )),
//       ValueDef(NamePattern("foo"), ClassNew(NamedType("Foo", Seq(NamedType("String", Nil))), Seq(
//         ValueDef(NamePattern("bar"), StringLiteral(Seq(StringLiteralPlainPart("foobar")))),
//         ValueDef(NamePattern("zot"), StringLiteral(Seq(StringLiteralPlainPart("foobar"))))
//       ))),
//       NamedValue("foo")
//     ))
//   }
//
//   err("Should not be allowed to add new members to calls during ::new")(Seq(
//     "Not allowed to declare new member here, definition for existing declaration expected"
//   ))(
//     Block(Seq(
//       ClassDef("Foo", Nil, None, Seq(
//         ValueDecl("bar", NamedType("String", Nil))
//       )),
//       ClassNew(NamedType("Foo", Nil), Seq(
//         ValueDef(NamePattern("bar"), StringLiteral(Seq(StringLiteralPlainPart("BAR")))),
//         ValueDef(NamePattern("zot"), StringLiteral(Seq(StringLiteralPlainPart("OOPS"))))
//       ))
//     ))
//   )
//
//   err("All members must be concrete in ::new")(Seq(
//     "Value member `baz` needs to be defined"
//   ))(
//     Block(Seq(
//       ClassDef("Foo", Nil, None, Seq(
//         ValueDecl("bar", NamedType("String", Nil)),
//         ValueDecl("baz", NamedType("String", Nil))
//       )),
//       ClassNew(NamedType("Foo", Nil), Seq(
//         ValueDef(NamePattern("bar"), StringLiteral(Seq(StringLiteralPlainPart("BAR"))))
//       ))
//     ))
//   )
//
//   err("::new should not work with type parameters")(Seq(
//     "Members not known for type parameter"
//   ))(
//     Block(Seq(
//       ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
//         ValueDef(NamePattern("bar"), ClassNew(NamedType("A", Nil), Nil))
//       ))
//     ))
//   )
//
//   err("::new should not work with recursive type reference")(Seq(
//     "Members not known for self-recursive type reference"
//   ))(
//     Block(Seq(
//       ClassDef("Foo", Nil, None, Seq(
//         ValueDef(NamePattern("bar"), ClassNew(NamedType("Foo", Nil), Nil))
//       ))
//     ))
//   )
//
//   err("::new should not work with top type")(Seq(
//     "Not found: type `⊤`"
//   ))(
//     Block(Seq(
//       ValueDef(NamePattern("foo"), ClassNew(NamedType("⊤", Nil), Nil))
//     ))
//   )
//
//   err("::new should not work with bottom type")(Seq(
//     "Not found: type `⊥`"
//   ))(
//     Block(Seq(
//       ValueDef(NamePattern("foo"), ClassNew(NamedType("⊥", Nil), Nil))
//     ))
//   )
//
// }
