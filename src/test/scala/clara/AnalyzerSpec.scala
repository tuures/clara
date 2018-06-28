package clara

import org.scalatest._

import ai.x.safe._

class AnalyzerSpec extends FunSuite {
  import Ast._

  def ve(testName: String, expectedSignature: String)(valueExpr: ValueExpr) = test(testName) {
    val blockWithPrelude = {
      // TODO have some useful test fixtures
      Block(Prelude.Prelude ++ Seq(valueExpr))
    }

    Analyzer.analyze(blockWithPrelude) match {
      case Right(t) => assert(t.signature(Analyzer.Env.empty) == expectedSignature)
      case Left(errors) => fail(errors.map(_.humanFormat).safeMkString("\n"))
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

}
