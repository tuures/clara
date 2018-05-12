package clara

import org.scalatest._

class AnalyzerSpec extends FunSuite {
  import Ast._

  val env = Stdlib.baseEnv

  test("Class definition and instantiation") {
    val bcs = Seq(
      ClassDef("Foo", Seq(TypeParam(Invariant, "A", 0)), None, Seq(
        ValueDecl("bar", NamedType("String")),
        ValueDecl("zot", NamedType("A"))
      )),
      ValueDef(NamePattern("foo"), ClassNew("Foo", Seq(NamedType("String")), Seq(
        ValueDef(NamePattern("bar"), StringLiteral("foobar")),
        ValueDef(NamePattern("zot"), StringLiteral("foobar"))
      ))),
      NamedValue("foo")
    )
    val s = Analyzer.Impl.walkBlockImpl(env)(bcs)
    s.result match {
      case Left(errors) =>
        throw new Exception(errors.mkString("\n"))
      case Right(t) =>
        assert(t.toSource(s.currentEnv) == "Foo")
    }
  }


}
