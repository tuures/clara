package clara

import org.scalatest._

import ai.x.safe._

class AnalyzerSpec extends FunSuite {
  import Ast._

  def ve(testName: String, expectedSignature: String)(valueExpr: ValueExpr) = test(testName) {
    Analyzer.analyze(Prelude.prependTo(valueExpr)) match {
      case Right(t) => assert(t.signature(Analyzer.Env.empty) == expectedSignature)
      case Left(errors) => fail(errors.map(_.humanFormat).safeMkString("\n"))
    }
  }

  def err(testName: String)(expectedErrorMessages: Seq[String])(valueExpr: ValueExpr) = test(testName) {
    Analyzer.analyze(Prelude.prependTo(valueExpr)) match {
      case Right(t) => fail("should not have been ok")
      case Left(errors) => assert(errors.map(_.message) == expectedErrorMessages)
    }
  }

  ve("Calling function yields result type", "()") {
    Call(Lambda(UnitPattern(), UnitLiteral()), UnitLiteral())
  }

  ve("Calling a higher-order function should infer type of the lambda that has no type annotation in the parameter pattern", "Int") {
    val strLength = Lambda(
      NamePattern("str"), // no type annotation
      MemberSelection(NamedValue("str"), "length", Nil)
    )

    val applyToHello = Lambda(
      // f: String => Int
      PatternAs(NamePattern("f"), FuncType(NamedType("String", Nil), NamedType("Int", Nil))),
      Call(NamedValue("f"), StringLiteral("Hello"))
    )

    Call(applyToHello, strLength)
  }

  ve("Block yields type of the last expression", "String") {
    Block(Seq(UnitLiteral(), StringLiteral("foo")))
  }

  ve("Selecting plain value member", "Int") {
    MemberSelection(StringLiteral("foo"), "length", Nil)
  }

  ve("Class definition and instantiation", "Foo[String]") {
    Block(Seq(
      ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
        ValueDecl("bar", NamedType("String", Nil)),
        ValueDecl("zot", NamedType("A", Nil))
      )),
      ValueDef(NamePattern("foo"), ClassNew(NamedType("Foo", Seq(NamedType("String", Nil))), Seq(
        ValueDef(NamePattern("bar"), StringLiteral("foobar")),
        ValueDef(NamePattern("zot"), StringLiteral("foobar"))
      ))),
      NamedValue("foo")
    ))
  }

  err("Should not be allowed to add new members to calls during ::new")(Seq(
    "Not allowed to declare new member here, definition for existing declaration expected"
  ))(
    Block(Seq(
      ClassDef("Foo", Nil, None, Seq(
        ValueDecl("bar", NamedType("String", Nil))
      )),
      ClassNew(NamedType("Foo", Nil), Seq(
        ValueDef(NamePattern("bar"), StringLiteral("BAR")),
        ValueDef(NamePattern("zot"), StringLiteral("OOPS"))
      ))
    ))
  )

  err("All members must be concrete in ::new")(Seq(
    "Value member `baz` needs to be defined"
  ))(
    Block(Seq(
      ClassDef("Foo", Nil, None, Seq(
        ValueDecl("bar", NamedType("String", Nil)),
        ValueDecl("baz", NamedType("String", Nil))
      )),
      ClassNew(NamedType("Foo", Nil), Seq(
        ValueDef(NamePattern("bar"), StringLiteral("BAR"))
      ))
    ))
  )

  err("::new should not work with type parameters")(Seq(
    "Members not known for type parameter"
  ))(
    Block(Seq(
      ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
        ValueDef(NamePattern("bar"), ClassNew(NamedType("A", Nil), Nil))
      ))
    ))
  )

  err("::new should not work with recursive type reference")(Seq(
    "Members not known for self-recursive type reference"
  ))(
    Block(Seq(
      ClassDef("Foo", Nil, None, Seq(
        ValueDef(NamePattern("bar"), ClassNew(NamedType("Foo", Nil), Nil))
      ))
    ))
  )

  err("::new should not work with top type")(Seq(
    "Not found: type `⊤`"
  ))(
    Block(Seq(
      ValueDef(NamePattern("foo"), ClassNew(NamedType("⊤", Nil), Nil))
    ))
  )

  err("::new should not work with bottom type")(Seq(
    "Not found: type `⊥`"
  ))(
    Block(Seq(
      ValueDef(NamePattern("foo"), ClassNew(NamedType("⊥", Nil), Nil))
    ))
  )

}
