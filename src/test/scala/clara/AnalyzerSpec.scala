package clara

import org.scalatest._

class AnalyzerSpec extends FunSuite {
  import Ast._

  test("Class definition and instantiation") {
    val bcs = Seq(
      ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
        ValueDecl("bar", NamedType("String", Nil)),
        ValueDecl("zot", NamedType("A", Nil))
      )),
      ValueDef(NamePattern("foo"), ClassNew(NamedType("Foo", Seq(NamedType("String", Nil))), Seq(
        ValueDef(NamePattern("bar"), StringLiteral("foobar")),
        ValueDef(NamePattern("zot"), StringLiteral("foobar"))
      ))),
      NamedValue("foo")
    )

    val blockWithPrelude = {
      Block(Prelude.Prelude ++ bcs)
    }

    Analyzer.analyze(blockWithPrelude) match {
      case Right(t) => assert(t.signature(Analyzer.Env.empty) == "Foo")
      case Left(errors) => throw new Exception(errors.mkString("\n"))
    }
  }


}
