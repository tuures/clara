package clara

import org.scalatest._

class AnalyzerSpec extends FunSuite {
  import Ast._

  val env = Stdlib.baseEnv

  def check(v: ValueExpr, expectedToSource: String) = {
    Analyzer.analyze(env)(v) match {
      case Left(errors) =>
        throw new Exception(errors.mkString("\n"))
      case Right(t) =>
        assert(t.toSource(env) == expectedToSource)
    }
  }

  def t(v: ValueExpr, expectedToSource: String): Unit = nt(expectedToSource)(v, expectedToSource)

  def nt(name: String)(v: ValueExpr, expectedToSource: String): Unit = test(name) { check(v, expectedToSource) }

  t(
    UnitLiteral(),
    "()"
  )

  nt("Block returning String")(
    Block(Seq(Block(Seq(
      ValueDef(NamePattern("foo"), StringLiteral("bar")),
      UnitLiteral(),
      NamedValue("foo")
    )))),
    "String"
  )

  test("Class2") {
    val bcs = Seq(
      ClassDef("Foo", Nil, None, Seq(
        AbstractMember("bar", NamedType("String"))
      )),
      ValueDef(NamePattern("foo"), ClassNew("Foo", Seq(
        ValueDef(NamePattern("bar"), StringLiteral("foobar"))
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
