package clara.parser

import clara.util.Safe._
import clara.ast.{Ast, LiteralValue}

import fastparse._
import org.scalatest.funsuite.AnyFunSuite
import scala.reflect.ClassTag

class ParserSpec extends AnyFunSuite {

  import Ast._

  def parseTest[T](parser: P[_] => P[T])(input: String)(expected: Any)(implicit ct: ClassTag[T]) = {
    test(safe"${ct.runtimeClass.getSimpleName()} ${input.replace("\n", "\\n")}") {
      parse(input, parser) match {
        case f: Parsed.Failure =>
          fail(input + "\n" + f.toString + "\n" + f.extra.trace().toString)
        case s: Parsed.Success[_] =>
          assert(s.value == expected)
          assert(s.index == input.length, "did not parse the whole input")
      }
    }
  }

  def err[T](parser: P[_] => P[T])(input: String)(implicit ct: ClassTag[T]) =
    test(safe"invalid: ${ct.runtimeClass.getSimpleName()} $input") {
      parse(input, parser) match {
        case Parsed.Success(_, index: Int) if index == input.length => fail("was parsed compeletely")
        case _ =>
      }
    }

  val p = ParserImpls(None)

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

  parseTest(p.unitLiteral(_))("()")(UnitLiteral())
  parseTest(p.unitType(_))(   "()")(UnitType())
  parseTest(p.unitPattern(_))("()")(UnitPattern())

  parseTest(p.floatLiteral(_))("3.14")(FloatLiteral(LiteralValue.Float("3", "14")))
  parseTest(p.floatLiteral(_))("1_000.123_456")(FloatLiteral(LiteralValue.Float("1000", "123456")))
  err(p.floatLiteral(_))("1._2")
  err(p.floatLiteral(_))("1_.2")

  parseTest(p.integerLiteral(_))("123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  // FIXME parse(p.integerLiteral, "-123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  parseTest(p.integerLiteral(_))("1_000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parseTest(p.integerLiteral(_))("1_\n000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parseTest(p.integerLiteral(_))("#x1a")(IntegerLiteral(LiteralValue.IntegerHex("1a")))
  // FIXME parse(p.integerLiteral, "-#x1a")(IntegerLiteral(LiteralValue.IntegerHex("1a")))
  parseTest(p.integerLiteral(_))("#x1A")(IntegerLiteral(LiteralValue.IntegerHex("1A")))
  parseTest(p.integerLiteral(_))("#b0010")(IntegerLiteral(LiteralValue.IntegerBin("0010")))
  // FIXME parse(p.integerLiteral, "-#b0010")(IntegerLiteral(LiteralValue.IntegerBin("0010")))
  err(p.integerLiteral(_))("1 0")
  err(p.integerLiteral(_))("1_")
  err(p.integerLiteral(_))("1__2")
  err(p.integerLiteral(_))("#\nx\n1")
  err(p.integerLiteral(_))("#x1g")
  err(p.integerLiteral(_))("#x1G")
  err(p.integerLiteral(_))("#x1ö")
  err(p.integerLiteral(_))("#b2")

  parseTest(p.processedStringLiteral(_))(""""str($foo)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str("),
    LiteralValue.StringExpressionPart(NamedValue("foo")),
    LiteralValue.StringPlainPart(")")
  )))
  parseTest(p.processedStringLiteral(_))("\" line1 \n line2\"")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" line1 \n line2")
  )))
  parseTest(p.processedStringLiteral(_))(""""str$(bar)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str"),
    LiteralValue.StringExpressionPart(NamedValue("bar"))
  )))
  parseTest(p.processedStringLiteral(_))(""""\"\\10\$\n"""")(StringLiteral(Seq(
    LiteralValue.StringEscapePart(Seq('"'.toString, """\""")),
    LiteralValue.StringPlainPart("10"),
    LiteralValue.StringEscapePart(Seq("$", "n"))
  )))
  err(p.processedStringLiteral(_))(""""10$"""")
  err(p.processedStringLiteral(_))(""""10$$"""")
  err(p.processedStringLiteral(_))(""""10\"""")
  err(p.processedStringLiteral(_))(""""10\ö"""")

  parseTest(p.verbatimStringLiteral(_))("' str '")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" str ")
  )))
  parseTest(p.verbatimStringLiteral(_))("'line1 \n line2'")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("line1 \n line2")
  )))
  parseTest(p.verbatimStringLiteral(_))("""'"'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart('"'.toString)
  )))
  parseTest(p.verbatimStringLiteral(_))("""'c:\n\$BAR\\a\'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("""c:\n\$BAR\\a\""")
  )))
  parseTest(p.verbatimStringLiteral(_))("""#'''#""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'")
  )))
  parseTest(p.verbatimStringLiteral(_))("""##''#'##""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'#")
  )))
  err(p.verbatimStringLiteral(_))("""'''""")

  parseTest(p.tuple(_))("((),())")(
    Tuple(Seq(UnitLiteral(), UnitLiteral()))
  )
  err(p.tuple(_))("()")

  parseTest(p.tupleType(_))("(() , ())")(
    TupleType(Seq(UnitType(), UnitType()))
  )
  parseTest(p.tuplePattern(_))("(\n(),\n(),\n)")(
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


  parseTest(p.parens(_))("((()))")(UnitLiteral())
  parseTest(p.parens(_))("((\n\n  ())\n)")(UnitLiteral())
  // nt("multi-line parens")(
  //   """|(
  //      |  1
  //      |)
  //      |""".stripMargin
  // )
  parseTest(p.typeParens(_))("((()))")(UnitType())
  parseTest(p.patternParens(_))("((()))")(UnitPattern())

  parseTest(p.block(_))("(\n\n// comment\n()\n;\n()\n();();;)")(
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
  err(p.block(_))("()")

  parseTest(p.namedValue(_))("foo")(NamedValue("foo"))

  parseTest(p.namedType(_))("Foo")(NamedType("Foo", Nil))
  // parse(p.namedType, "Foo[Bar]")(
  //   NamedType("Foo", Seq(NamedType("Bar"/*, Nil*/)))
  // )
  // parse(p.namedType, "Foo[Bar, Baz[Zot]]")(
  //   NamedType("Foo", Seq(
  //     NamedType("Bar", Nil),
  //     NamedType("Baz", Seq(NamedType("Zot"/*, Nil*/))))
  //   )
  // )

  parseTest(p.namePattern(_))("foo")(NamePattern("foo"))

  parseTest(p.valueAs(_))("foo: String")(
    ValueAs(NamedValue("foo"), NamedType("String", Nil))
  )

  parseTest(p.record(_))("{}")(Record(Nil))
  parseTest(p.record(_))("{ a = foo, b: Bar = bar }")(Record(Seq(
    FieldDef("a", None, NamedValue("foo")),
    FieldDef("b", Some(NamedType("Bar", Nil)), NamedValue("bar"))
  )))

  parseTest(p.recordType(_))("{}")(RecordType(Nil))
  parseTest(p.recordType(_))("{ a: Foo, b: Bar }")(RecordType(Seq(
    FieldDecl("a", NamedType("Foo", Nil)),
    FieldDecl("b", NamedType("Bar", Nil))
  )))

  parseTest(p.lambda(_))("() => ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parseTest(p.lambda(_))("() =>\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parseTest(p.lambda(_))("() =>//comment\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  // nt("multi-line function")(
  //   """|a = () =>
  //      |  1.squared
  //      |2
  //      |""".stripMargin
  // )

  parseTest(p.funcType(_))("() => ()")(
    FuncType(UnitType(), UnitType())
  )

  parseTest(p.memberOrCall(_))("foo.length.toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedMember("length"/*, Nil*/)), NamedMember("toString"/*, Nil*/))
  )
  // parse(p.memberOrCall, "foo.bar[Zot]")(
  //   MemberSelection(NamedValue("foo"), NamedMember("bar", Seq(NamedType("Zot"/*, Nil*/))))
  // )
  parseTest(p.memberOrCall(_))("foo.\n  length.\n  toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedMember("length"/*, Nil*/)), NamedMember("toString"/*, Nil*/))
  )
  // nt("multi-line member")(
  //   """|123.
  //      |  squared.
  //      |  doubled.
  //      |  sqrt
  //      |""".stripMargin
  // )
  parseTest(p.memberOrCall(_))("foo()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parseTest(p.memberOrCall(_))("foo ()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parseTest(p.memberOrCall(_))("foo bar")(
    Call(NamedValue("foo"), NamedValue("bar"))
  )

  parseTest(p.attributes(_))("@[a b]@[c]")(Seq(Attribute("a", Some("b")), Attribute("c", None)))
  parseTest(p.attributes(_))("@[a]\n@[c]\n")(Seq(Attribute("a", None), Attribute("c", None)))

  parseTest(p.valueDecl(_))("::declare ::val foo: ()")(ValueDecl("foo", UnitType()))

  parseTest(p.valueNamesDef(_))("a =\u0020\u0020\n\u0020\u0020\n  foo")(
    ValueNamesDef(NamePattern("a"), NamedValue("foo"))
  )

  parseTest(p.aliasTypeDef(_))("::alias Foo = Bar")(AliasTypeDef("Foo", Nil, NamedType("Bar", Nil)))

  parseTest(p.typeDef(_))("::declare ::type Foo = Bar")(TypeDef(true, "Foo", Nil, NamedType("Bar", Nil)))

  parseTest(p.typeDef(_))("::type Foo = Bar")(TypeDef(false, "Foo", Nil, NamedType("Bar", Nil)))

  parseTest(p.newExpr(_))("::new Foo")(NewExpr(NamedType("Foo", Nil)))

  parseTest(p.methodDeclSection(_))("::declare ::methods Bar { foo: Bar }")(
    MethodDeclSection(TypeName("Bar"), Seq(
      MethodDecl(Nil, "foo", NamedType("Bar", Nil))
    ))
  )
  parseTest(p.methodDeclSection(_))("::declare ::methods Bar { foo: Bar = sic }")(
    MethodDeclSection(TypeName("Bar"), Seq(
      MethodDef(Nil, "foo", Some(NamedType("Bar", Nil)), NamedValue("sic"))
    ))
  )
  parseTest(p.methodDeclSection(_))("::declare ::methods Bar {\n@[a]\nfoo: Bar\nbaz: Baz\n}")(
    MethodDeclSection(TypeName("Bar"), Seq(
      MethodDecl(Seq(Attribute("a", None)), "foo", NamedType("Bar", Nil)),
      MethodDecl(Nil, "baz", NamedType("Baz", Nil)),
    ))
  )

  // parseTest(p.methodDefSection(_))("::methods b: Bar { foo = b }")(
  //   MethodDefSection(PatternAs(NamePattern("b"), NamedType("Bar", Nil)), Seq(
  //     MethodDef(Nil, "foo", None, NamedValue("b"))
  //   ))
  // )
  // parseTest(p.methodDefSection(_))("::methods b: Bar {\nfoo: Bar = b\n}")(
  //   MethodDefSection(PatternAs(NamePattern("b"), TypeName("Bar")), Seq(
  //     MethodDef(Nil, "foo", Some(NamedType("Bar", Nil)), NamedValue("b"))
  //   ))
  // )
  // parseTest(p.methodDefSection(_))("::methods b: Bar { foo: Sic }")(
  //   MethodDefSection(PatternAs(NamePattern("b"), TypeName("Bar")), Seq(
  //     MethodDecl(Nil, "foo", NamedType("Sic", Nil))
  //   ))
  // )

  parseTest(p.programBlock(_))("(foo/*comment\n /*  \n // line2*/)")(
    Block(Seq(NamedValue("foo")))
  )
  parseTest(p.programBlock(_))("foo//\n")(
    Block(Seq(NamedValue("foo")))
  )
  parseTest(p.programBlock(_))("foo//")(
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

  parseTest(p.valueExpr(_))("1234")(IntegerLiteral(LiteralValue.IntegerDec("1234")))
  err(p.valueExpr(_))("foo square = 1")
  err(p.valueExpr(_))("1square = 1")
}
