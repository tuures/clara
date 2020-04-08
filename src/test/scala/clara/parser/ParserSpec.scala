package clara.parser

import ai.x.safe._
import fastparse.all.{P, Parsed}
import org.scalatest.FunSuite

import clara.ast.{Ast, LiteralValue}

class ParserSpec extends FunSuite {

  import Ast._

  def parse[R](parser: P[R], input: String)(expected: Any) = test(safe"valid: ${parser.toString} ${input.replace("\n", "\\n")}") {
    parser.parse(input) match {
      case f: Parsed.Failure =>
        fail(input + "\n" + f.toString + "\n" + f.extra.traced.toString)
      case s: Parsed.Success[_] =>
        assert(s.value == expected)
        assert(s.index == input.length, "did not parse the whole input")
    }
  }

  def err[R](parser: P[R], input: String) = test(safe"invalid: ${parser.toString} $input") {
    parser.parse(input) match {
      case Parsed.Success(_, index: Int) if index == input.length => fail("was parsed compeletely")
      case _ =>
    }
  }

  val p = Parser.Impl(None)

  // TODO migrate:
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
  // nt("block as argument")(
  //   """|foo (
  //      |  bar
  //      |  baz
  //      |)""".stripMargin
  // )

  parse(p.unitLiteral, "()")(UnitLiteral())
  parse(p.unitType,    "()")(UnitType())
  parse(p.unitPattern, "()")(UnitPattern())

  parse(p.floatLiteral, "3.14")(FloatLiteral(LiteralValue.Float("3", "14")))
  parse(p.floatLiteral, "1_000.123_456")(FloatLiteral(LiteralValue.Float("1000", "123456")))
  err(p.floatLiteral, "1._2")
  err(p.floatLiteral, "1_.2")

  parse(p.integerLiteral, "123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  // parse(p.integerLiteral, "-123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  parse(p.integerLiteral, "1_000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parse(p.integerLiteral, "1_\n000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parse(p.integerLiteral, "#x1a")(IntegerLiteral(LiteralValue.IntegerHex("1a")))
  // parse(p.integerLiteral, "-#x1a")(IntegerLiteral(LiteralValue.IntegerHex("1a")))
  parse(p.integerLiteral, "#x1A")(IntegerLiteral(LiteralValue.IntegerHex("1A")))
  parse(p.integerLiteral, "#b0010")(IntegerLiteral(LiteralValue.IntegerBin("0010")))
  // parse(p.integerLiteral, "-#b0010")(IntegerLiteral(LiteralValue.IntegerBin("0010")))
  err(p.integerLiteral, "1 0")
  err(p.integerLiteral, "1_")
  err(p.integerLiteral, "1__2")
  err(p.integerLiteral, "#\nx\n1")
  err(p.integerLiteral, "#x1g")
  err(p.integerLiteral, "#x1G")
  err(p.integerLiteral, "#x1ö")
  err(p.integerLiteral, "#b2")

  parse(p.processedStringLiteral, """"str($foo)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str("),
    LiteralValue.StringExpressionPart(NamedValue("foo")),
    LiteralValue.StringPlainPart(")")
  )))
  parse(p.processedStringLiteral, "\" line1 \n line2\"")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" line1 \n line2")
  )))
  parse(p.processedStringLiteral, """"str$(bar)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str"),
    LiteralValue.StringExpressionPart(NamedValue("bar"))
  )))
  parse(p.processedStringLiteral, """"\"\\10\$\n"""")(StringLiteral(Seq(
    LiteralValue.StringEscapePart(Seq('"'.toString, """\""")),
    LiteralValue.StringPlainPart("10"),
    LiteralValue.StringEscapePart(Seq("$", "n"))
  )))
  err(p.processedStringLiteral, """"10$"""")
  err(p.processedStringLiteral, """"10$$"""")
  err(p.processedStringLiteral, """"10\"""")
  err(p.processedStringLiteral, """"10\ö"""")

  parse(p.verbatimStringLiteral, "' str '")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" str ")
  )))
  parse(p.verbatimStringLiteral, "'line1 \n line2'")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("line1 \n line2")
  )))
  parse(p.verbatimStringLiteral, """'"'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart('"'.toString)
  )))
  parse(p.verbatimStringLiteral, """'c:\n\$BAR\\a\'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("""c:\n\$BAR\\a\""")
  )))
  parse(p.verbatimStringLiteral, """#'''#""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'")
  )))
  parse(p.verbatimStringLiteral, """##''#'##""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'#")
  )))
  err(p.verbatimStringLiteral, """'''""")

  parse(p.tuple, "((),())")(
    Tuple(Seq(UnitLiteral(), UnitLiteral()))
  )
  err(p.tuple, "()")

  parse(p.tupleType, "(() , ())")(
    TupleType(Seq(UnitType(), UnitType()))
  )
  parse(p.tuplePattern, "(\n(),\n(),\n)")(
    TuplePattern(Seq(UnitPattern(), UnitPattern()))
  )
  // nt("multi-line tuple")(
  //   """|(
  //      |  1,
  //      |  2
  //      |)""".stripMargin
  // )
  // nt("multi-line tuple, trailing comma")(
  //   """|(
  //      |  1,
  //      |  2,
  //      |)""".stripMargin,
  //   Some(Block(Seq(Tuple(Seq(IntegerLiteral("1"), IntegerLiteral("2"))))))
  // )


  parse(p.parens, "((()))")(UnitLiteral())
  parse(p.parens, "((\n\n  ())\n)")(UnitLiteral())
  // nt("multi-line parens")(
  //   """|(
  //      |  1
  //      |)
  //      |""".stripMargin
  // )
  parse(p.typeParens, "((()))")(UnitType())
  parse(p.patternParens, "((()))")(UnitPattern())

  parse(p.block, "(\n\n// comment\n()\n;\n()\n();();;)")(
    Block(Seq.fill(4)(UnitLiteral()))
  )
  // nt("simple block")(
  //   """|(
  //      |  foo
  //      |  bar
  //      |)""".stripMargin,
  //   Some(Block(Seq(Block(Seq(NamedValue("foo"), NamedValue("bar"))))))
  // )
  // nt("semicolon block")(
  //   """|(
  //      |  foo; bar;
  //      |  baz;;
  //      |  xyzzy
  //      |)""".stripMargin
  // )
  err(p.block, "()")

  parse(p.namedValue, "foo")(NamedValue("foo"))

  parse(p.namedType, "Foo")(NamedType("Foo"/*, Nil*/))
  // parse(p.namedType, "Foo[Bar]")(
  //   NamedType("Foo", Seq(NamedType("Bar"/*, Nil*/)))
  // )
  // parse(p.namedType, "Foo[Bar, Baz[Zot]]")(
  //   NamedType("Foo", Seq(
  //     NamedType("Bar", Nil),
  //     NamedType("Baz", Seq(NamedType("Zot"/*, Nil*/))))
  //   )
  // )

  parse(p.namePattern, "foo")(NamePattern("foo"))

  parse(p.valueAs, "foo: String")(
    ValueAs(NamedValue("foo"), NamedType("String"/*, Nil*/))
  )

  parse(p.record, "{}")(Record(Nil))
  parse(p.record, "{ a = foo, b: Bar = bar }")(Record(Seq(
    FieldDef("a", None, NamedValue("foo")),
    FieldDef("b", Some(NamedType("Bar")), NamedValue("bar"))
  )))

  parse(p.recordType, "{}")(RecordType(Nil))
  parse(p.recordType, "{ a: Foo, b: Bar }")(RecordType(Seq(
    FieldDecl("a", NamedType("Foo")),
    FieldDecl("b", NamedType("Bar"))
  )))

  parse(p.lambda, "() => ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parse(p.lambda, "() =>\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parse(p.lambda, "() =>//comment\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  // nt("multi-line function")(
  //   """|a = () =>
  //      |  1.squared
  //      |2
  //      |""".stripMargin
  // )

  parse(p.funcType, "() => ()")(
    FuncType(UnitType(), UnitType())
  )

  parse(p.memberOrCall, "foo.length.toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedMember("length"/*, Nil*/)), NamedMember("toString"/*, Nil*/))
  )
  // parse(p.memberOrCall, "foo.bar[Zot]")(
  //   MemberSelection(NamedValue("foo"), NamedMember("bar", Seq(NamedType("Zot"/*, Nil*/))))
  // )
  parse(p.memberOrCall, "foo.\n  length.\n  toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedMember("length"/*, Nil*/)), NamedMember("toString"/*, Nil*/))
  )
  // nt("multi-line member")(
  //   """|123.
  //      |  squared.
  //      |  doubled.
  //      |  sqrt
  //      |""".stripMargin
  // )
  parse(p.memberOrCall, "foo()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parse(p.memberOrCall, "foo ()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parse(p.memberOrCall, "foo bar")(
    Call(NamedValue("foo"), NamedValue("bar"))
  )

  parse(p.attributes, "@[a b]@[c]")(Seq(Attribute("a", Some("b")), Attribute("c", None)))
  parse(p.attributes, "@[a]\n@[c]\n")(Seq(Attribute("a", None), Attribute("c", None)))

  parse(p.valueDecl, "::declare ::val foo: ()")(ValueDecl("foo", UnitType()))

  parse(p.valueNamesDef, "a =\u0020\u0020\n\u0020\u0020\n  foo")(
    ValueNamesDef(NamePattern("a"), NamedValue("foo"))
  )

  parse(p.aliasTypeDef, "::alias Foo = Bar")(AliasTypeDef("Foo", NamedType("Bar")))

  parse(p.typeDef, "::declare ::type Foo = Bar")(TypeDef(true, "Foo", NamedType("Bar")))

  parse(p.typeDef, "::type Foo = Bar")(TypeDef(false, "Foo", NamedType("Bar")))

  parse(p.newExpr, "::new Foo")(NewExpr(NamedType("Foo")))

  parse(p.methodDeclSection, "::declare ::methods Bar { foo: Bar }")(
    MethodDeclSection(NamedType("Bar"), Seq(
      MethodDecl(Nil, "foo", NamedType("Bar"))
    ))
  )
  parse(p.methodDeclSection, "::declare ::methods Bar { foo: Bar = sic }")(
    MethodDeclSection(NamedType("Bar"), Seq(
      MethodDef(Nil, "foo", Some(NamedType("Bar")), NamedValue("sic"))
    ))
  )
  parse(p.methodDeclSection, "::declare ::methods Bar {\n@[a]\nfoo: Bar\nbaz: Baz\n}")(
    MethodDeclSection(NamedType("Bar"), Seq(
      MethodDecl(Seq(Attribute("a", None)), "foo", NamedType("Bar")),
      MethodDecl(Nil, "baz", NamedType("Baz")),
    ))
  )

  parse(p.methodDefSection, "::methods b: Bar { foo = b }")(
    MethodDefSection(PatternAs(NamePattern("b"), NamedType("Bar")), Seq(
      MethodDef(Nil, "foo", None, NamedValue("b"))
    ))
  )
  parse(p.methodDefSection, "::methods b: Bar {\nfoo: Bar = b\n}")(
    MethodDefSection(PatternAs(NamePattern("b"), NamedType("Bar")), Seq(
      MethodDef(Nil, "foo", Some(NamedType("Bar")), NamedValue("b"))
    ))
  )
  parse(p.methodDefSection, "::methods b: Bar { foo: Sic }")(
    MethodDefSection(PatternAs(NamePattern("b"), NamedType("Bar")), Seq(
      MethodDecl(Nil, "foo", NamedType("Sic"))
    ))
  )

  parse(p.program, "(foo/*comment\n /*  \n // line2*/)")(
    Block(Seq(NamedValue("foo")))
  )
  parse(p.program, "foo//\n")(
    Block(Seq(NamedValue("foo")))
  )
  parse(p.program, "foo//")(
    Block(Seq(NamedValue("foo")))
  )

  // nt("multi-line assignment with extra spaces")(
  //   """|a =\u0020\u0020
  //      |\u0020\u0020
  //      |  foo
  //      |""".stripMargin
  // )

  // FIXME
  // parse(p.typeDef, "::type String = _")(TypeDef("String"))

  // parse(p.classDef, "::class Book {isbn: String, desc: String}")(
  //   ClassDef("Book", Nil, None, Seq(
  //     ValueDecl("isbn", NamedType("String", Nil)),
  //     ValueDecl("desc", NamedType("String", Nil))
  //   ))
  // )
  // nt("single-line class")(
  //   "::class Book {isbn: String, author: String, title: String}"
  // )

  // parse(p.classDef, "::class Book[+D] {\n  isbn: String,\n  desc: String,\n}")(
  //   ClassDef("Book", Seq(TypeParam(Covariant, "D", 0)), None, Seq(
  //     ValueDecl("isbn", NamedType("String", Nil)),
  //     ValueDecl("desc", NamedType("String", Nil))
  //   ))
  // )
  // nt("class with simple type params and value declarations")(
  //   "::class Book[A] {isbn: String, author: String, title: String}"
  // )
  // nt("multi-line class")(
  //   """|::class Book {
  //      |  isbn: String
  //      |  author: String
  //      |\u0020\u0020
  //      |  title: String,
  //      |}
  //      |""".stripMargin
  // )

  // TODO migrate:
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
  // nt("complex type params and method declarations")(
  //   "::class Functor[A, M[_]] { ::method map[B]: (A => B) => M[B] }"
  // )

  // parse(p.classNew, "::new Foo {bar = 1, zot = 2}")(
  //   ClassNew(NamedType("Foo", Nil), Seq(
  //     ValueDef(NamePattern("bar"), IntegerLiteral(LiteralValue.IntegerDec("1"))),
  //     ValueDef(NamePattern("zot"), IntegerLiteral(LiteralValue.IntegerDec("2")))
  //   ))
  // )
  // t("::new Foo {}")

  parse(p.valueExpr, "123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  err(p.valueExpr, "foo square = 1")
  err(p.valueExpr, "1square = 1")
}
