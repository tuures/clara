package clara

import org.scalatest._

import fastparse.all._

class ValidSyntaxSpec extends FunSuite {

  import Parser._

  def check(input: String, expected: Option[Parser.Node]) = {
    Parser.parse(input) match {
      case f: Parsed.Failure =>
        throw new Exception(input + "\n" + f.extra.traced.fullStack)
      case s: Parsed.Success[_] =>
        assert(s.index == input.length)
        expected.foreach(n => assert(s.value == n))
    }
  }

  def t(input: String, expected: Option[Parser.Node] = None): Unit = nt(input)(input, expected)

  def nt(name: String)(input: String, expected: Option[Parser.Node] = None): Unit = test(name) { check(input, expected) }

  t("foo")
  t("()")
  t("(): ()")
  t("'str'")
  t("(b, a, 1, 'foobar')")
  t("() => 1")
  t("a => ()")
  t("(a, b) => a")
  t("(a: Int, b: String) => (b, a, 1, 'foobar')")
  t("(a, b): (Int, Int) => 1")
  t("(a: Int, (b: Int, c: Int)) => 'str'")
  t("let square = (n: Int) => Math.power(n, 2)")
  t("let (a, b, c) = u")
  t("let add = (a: Int) => (b: Int) => Math.add(a, b)")
  t("() => foo()")
  t("() => bar")
  t("let a = (1, 2.squared, ('bär'.toUpper('FI'), 'baz', () => (r, s)))")
  t("1: Double")
  t("(1, 2): (Int, Int)")
  t("(1, 2): ((Int, Int))")
  t("(() => foo()): (() => Foo)")
  t("((a: Int, b: Int) => (1, (2, 3))): ((Int, Int) => (Int, (Int, Int)))")
  t("foo .bar baz")
  t("foo 'asd'")
  t("foo ()")
  t("foo 1")

  nt("simple block")(
    """|(
       |  foo
       |  bar
       |)""".stripMargin,
    Some(Block(Seq(Block(Seq(NamedValue("foo"), NamedValue("bar"))))))
  )

  nt("semicolon block")(
    """|(
       |  foo; bar;
       |  baz;;
       |  xyzzy
       |)""".stripMargin
  )

  nt("one block program")(
    """|let bl = (
       |  let x = 5; let y = 6
       |  let a = (1, 2)
       |  log(a, x)
       |  let b = (a, Int, b: Int) => (a, b, c)
       |
       |  b
       |): (Int, Int, Int)
       |let (bla, blb, blc) = bl
       |bla.squared
       |""".stripMargin
  )

  nt("block as argument")(
    """|foo (
       |  bar
       |  baz
       |)""".stripMargin
  )

  nt("multi-line tuple")(
    """|(
       |  1,
       |  2
       |)""".stripMargin
  )

  nt("multi-line tuple, trailing comma")(
    """|(
       |  1,
       |  2,
       |)""".stripMargin,
    Some(Block(Seq(Tuple(Seq(IntegerLiteral("1"), IntegerLiteral("2"))))))
  )

  nt("multi-line lambda")(
    """|let a = () =>
       |  1.squared
       |2
       |""".stripMargin
  )

  nt("multi-line parens")(
    """|(
       |  1
       |)
       |""".stripMargin
  )

  nt("multi-line member")(
    """|123.
       |  squared.
       |  doubled.
       |  sqrt
       |""".stripMargin
  )

  nt("multi-line assignment with extra spaces")(
    """|let a =\u0020\u0020
       |\u0020\u0020
       |  foo
       |""".stripMargin
  )
}
