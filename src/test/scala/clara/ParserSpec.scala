package clara

import org.scalatest._

import fastparse.all._

class ParserSpec extends FunSuite {

  import Ast._

  def parse[R](parser: Parser[R], input: String)(expected: Node) = test(s"valid: ${parser.toString} ${input.replace("\n", "\\n")}") {
    parser.parse(input) match {
      case f: Parsed.Failure =>
        fail(input + "\n" + f.toString + "\n" + f.extra.traced.toString)
      case s: Parsed.Success[_] =>
        assert(s.index == input.length)
        assert(s.value == expected)
    }
  }

  def err[R](parser: Parser[R], input: String) = test(s"invalid: ${parser.toString} $input") {
    parser.parse(input) match {
      case Parsed.Success(_, index: Int) if index == input.length => fail("was parsed compeletely")
      case _ =>
    }
  }

  parse(Parser.Impl.unitLiteral, "()")(UnitLiteral())
  parse(Parser.Impl.unitType,    "()")(UnitType())
  parse(Parser.Impl.unitPattern, "()")(UnitPattern())

  parse(Parser.Impl.integerLiteral, "123")(IntegerLiteral("123"))

  parse(Parser.Impl.stringLiteral, "'str'")(StringLiteral("str"))

  parse(Parser.Impl.tuple,        "((),())")(
    Tuple(Seq(UnitLiteral(), UnitLiteral()))
  )
  err(Parser.Impl.tuple, "()")

  parse(Parser.Impl.tupleType,    "(() , ())")(
    TupleType(Seq(UnitType(), UnitType()))
  )
  parse(Parser.Impl.tuplePattern, "(\n(),\n(),\n)")(
    TuplePattern(Seq(UnitPattern(), UnitPattern()))
  )

  parse(Parser.Impl.parens, "((()))")(UnitLiteral())
  parse(Parser.Impl.typeParens, "((()))")(UnitType())
  parse(Parser.Impl.patternParens, "((()))")(UnitPattern())

  parse(Parser.Impl.block, "(\n\n()\n;\n()\n())")(
    Block(Seq.fill(3)(UnitLiteral()))
  )
  err(Parser.Impl.block, "()")

  parse(Parser.Impl.namedValue, "foo")(NamedValue("foo"))

  parse(Parser.Impl.namedType, "Foo")(NamedType("Foo", Nil))
  parse(Parser.Impl.namedType, "Foo[Bar]")(
    NamedType("Foo", Seq(NamedType("Bar", Nil)))
  )
  parse(Parser.Impl.namedType, "Foo[Bar, Baz[Zot]]")(
    NamedType("Foo", Seq(
      NamedType("Bar", Nil),
      NamedType("Baz", Seq(NamedType("Zot", Nil))))
    )
  )

  parse(Parser.Impl.namePattern, "foo")(NamePattern("foo"))

  parse(Parser.Impl.valueAs, "foo: String")(
    ValueAs(NamedValue("foo"), NamedType("String", Nil))
  )

  parse(Parser.Impl.lambda, "() => ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )

  parse(Parser.Impl.funcType, "() => ()")(
    FuncType(UnitType(), UnitType())
  )

  parse(Parser.Impl.memberOrCall, "foo.length")(
    MemberSelection(NamedValue("foo"), "length", Nil)
  )
  parse(Parser.Impl.memberOrCall, "foo.bar[Zot]")(
    MemberSelection(NamedValue("foo"), "bar", Seq(NamedType("Zot", Nil)))
  )
  parse(Parser.Impl.memberOrCall, "foo()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parse(Parser.Impl.memberOrCall, "foo ()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parse(Parser.Impl.memberOrCall, "foo bar")(
    Call(NamedValue("foo"), NamedValue("bar"))
  )

  parse(Parser.Impl.program, "(foo)")(
    Block(Seq(NamedValue("foo")))
  )

  // t("(a: Int, b: String) => (b, a, 1, 'foobar')")
  // t("(a, b): (Int, Int) => 1")
  // t("(a: Int, (b: Int, c: Int)) => 'str'")
  // t("square = (n: Int) => Math.power(n, 2)")
  // t("(a, b, c) = u")
  // t("add = (a: Int) => (b: Int) => Math.add(a, b)", Some(
  //   Block(Seq(
  //     ValueDef(NamePattern("add"),
  //       Lambda(PatternAs(NamePattern("a"), NamedType("Int", Nil)),
  //         Lambda(PatternAs(NamePattern("b"), NamedType("Int", Nil)),
  //           Call(
  //             MemberSelection(NamedValue("Math"), "add", Nil),
  //             Tuple(Seq(NamedValue("a"), NamedValue("b")))
  //           )
  //         )
  //       )
  //     )
  //   ))
  // ))
  // t("() => foo()")
  // t("() => bar")
  // t("a = (1, 2.squared, ('bär'.toUpper('FI'), 'baz', () => (r, s)))")
  // t("1: Double")
  // t("(1, 2): (Int, Int)")
  // t("(1, 2): ((Int, Int))")
  // t("(() => foo()): (() => Foo)")
  // t("((a: Int, b: Int) => (1, (2, 3))): ((Int, Int) => (Int, (Int, Int)))")
  // t("foo .bar baz")
  // t("foo 'asd'")
  // t("foo ()")
  // t("foo 1")
  //
  // nt("simple block")(
  //   """|(
  //      |  foo
  //      |  bar
  //      |)""".stripMargin,
  //   Some(Block(Seq(Block(Seq(NamedValue("foo"), NamedValue("bar"))))))
  // )
  //
  // nt("semicolon block")(
  //   """|(
  //      |  foo; bar;
  //      |  baz;;
  //      |  xyzzy
  //      |)""".stripMargin
  // )
  //
  // nt("one block program")(
  //   """|bl = (
  //      |  x = 5; y = 6
  //      |  a = (1, 2)
  //      |  log(a, x)
  //      |  b = (a, Int, b: Int) => (a, b, c)
  //      |
  //      |  b(x, y, 3)
  //      |): (Int, Int, Int)
  //      |(bla, blb, blc) = bl
  //      |bla.squared
  //      |""".stripMargin
  // )
  //
  // nt("block as argument")(
  //   """|foo (
  //      |  bar
  //      |  baz
  //      |)""".stripMargin
  // )
  //
  // nt("multi-line tuple")(
  //   """|(
  //      |  1,
  //      |  2
  //      |)""".stripMargin
  // )
  //
  // nt("multi-line tuple, trailing comma")(
  //   """|(
  //      |  1,
  //      |  2,
  //      |)""".stripMargin,
  //   Some(Block(Seq(Tuple(Seq(IntegerLiteral("1"), IntegerLiteral("2"))))))
  // )
  //
  // nt("multi-line function")(
  //   """|a = () =>
  //      |  1.squared
  //      |2
  //      |""".stripMargin
  // )
  //
  // nt("multi-line parens")(
  //   """|(
  //      |  1
  //      |)
  //      |""".stripMargin
  // )
  //
  // nt("multi-line member")(
  //   """|123.
  //      |  squared.
  //      |  doubled.
  //      |  sqrt
  //      |""".stripMargin
  // )
  //
  // nt("multi-line assignment with extra spaces")(
  //   """|a =\u0020\u0020
  //      |\u0020\u0020
  //      |  foo
  //      |""".stripMargin
  // )
  //
  // nt("single-line class")(
  //   "::class Book {isbn: String, author: String, title: String}"
  // )
  //
  // nt("multi-line class")(
  //   """|::class Book {
  //      |  isbn: String
  //      |  author: String
  //      |\u0020\u0020
  //      |  title: String,
  //      |}
  //      |""".stripMargin
  // )
  //
  // nt("class fields and methods")(
  //   """|::class Foo << Bar {
  //      |  yyy: Int = 2
  //      |  yyy = 3
  //      |  sum: Int (i: Int, j: Int) = i .plus j
  //      |  xxx: Int (i: Int) = i
  //      |  yyy: Int = 2
  //      |  sum(i: Int, j: Int) = i .plus j
  //      |  xxx(i: Int) = i
  //      |  xxx i: Int = i
  //      |  yyy = 3
  //      |}
  //      |""".stripMargin
  // )
  //
  // t("::new Foo {}")
  //
  // nt("class with simple type params and value declarations")(
  //   "::class Book[A] {isbn: String, author: String, title: String}"
  // )
  //
  // nt("complex type params and method declarations")(
  //   "::class Functor[A, M[_]] { ::method map[B]: (A => B) => M[B] }"
  // )

  err(Parser.Impl.valueExpr, "foo square = 1")
  err(Parser.Impl.valueExpr, "1square = 1")
}
