package clara.parser

import clara.util.Safe.SafeStringContext
import clara.ast.{Ast, LiteralValue}
import clara.testutil.{AstTestHelpers, BaseSpec}

import scala.reflect.ClassTag

class ParserImplsSpec extends BaseSpec {
  import Ast.{TypeDef => _, NamedType => _, Lambda => _, FuncType => _, _}
  import AstTestHelpers._

  import fastparse._

  def typeName[T](implicit ct: ClassTag[T]) = ct.runtimeClass.getSimpleName()
  def escapeInputForName(s: String) = s.replace("\n", "\\n")

  /** tests that the give sub-parser can parse given input into given AST */
  def parseAst[T](parser: P[_] => P[T])(input: String)(expected: Any)(implicit ct: ClassTag[T]) = {
    test(safe"${typeName} ${escapeInputForName(input)}") {
      parse(input, parser) match {
        case f: Parsed.Failure =>
          fail("Failed to parse: " + f.trace().longMsg)
        case s: Parsed.Success[_] =>
          assert(s.value === expected)
          assert(s.index === input.length, "Did not parse the whole input")
        }
    }
  }

  /** tests that the given sub-parser rejects the given input */
  def reject[T](parser: P[_] => P[T])(input: String)(implicit ct: ClassTag[T]) =
    test(safe"reject ${typeName} ${escapeInputForName(input)}") {
      parse(input, parser) match {
        case Parsed.Success(value, index) if index === input.length =>
          fail(safe"Should have been rejected but parsed as ${value.toString()}")
        case _ => ()
      }
    }

  val p = ParserImpls(None)

  //////
  // Literals

  parseAst(p.topType(_))("*")(TopType())
  parseAst(p.bottomType(_))("!")(BottomType())

  parseAst(p.unitLiteral(_))("()")(UnitLiteral())
  parseAst(p.unitType(_))(   "()")(UnitType())
  parseAst(p.unitPattern(_))("()")(UnitPattern())

  parseAst(p.floatLiteral(_))("3.14")(FloatLiteral(LiteralValue.Float("3", "14")))
  parseAst(p.floatLiteral(_))("1_000.123_456")(FloatLiteral(LiteralValue.Float("1000", "123456")))
  parseAst(p.floatLiteral(_))("-1.1")(FloatLiteral(LiteralValue.Float("-1", "1")))
  reject(p.floatLiteral(_))("_100.0")
  reject(p.floatLiteral(_))("1._2")
  reject(p.floatLiteral(_))("1.-2")
  reject(p.floatLiteral(_))("--1.2")
  reject(p.floatLiteral(_))("- 1.2")
  reject(p.floatLiteral(_))("1_.2")

  parseAst(p.integerLiteral(_))("123")(IntegerLiteral(LiteralValue.IntegerDec("123")))
  parseAst(p.integerLiteral(_))("-123")(IntegerLiteral(LiteralValue.IntegerDec("-123")))
  parseAst(p.integerLiteral(_))("1_000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parseAst(p.integerLiteral(_))("1_\n000")(IntegerLiteral(LiteralValue.IntegerDec("1000")))
  parseAst(p.integerLiteral(_))("#x1a")(IntegerLiteral(LiteralValue.IntegerHex("1a")))
  parseAst(p.integerLiteral(_))("-#x1a")(IntegerLiteral(LiteralValue.IntegerHex("-1a")))
  parseAst(p.integerLiteral(_))("#x1A")(IntegerLiteral(LiteralValue.IntegerHex("1A")))
  parseAst(p.integerLiteral(_))("#b0010")(IntegerLiteral(LiteralValue.IntegerBin("0010")))
  parseAst(p.integerLiteral(_))("-#b0010")(IntegerLiteral(LiteralValue.IntegerBin("-0010")))
  reject(p.integerLiteral(_))("1 0")
  reject(p.integerLiteral(_))("_100")
  reject(p.integerLiteral(_))("1_")
  reject(p.integerLiteral(_))("1__2")
  reject(p.integerLiteral(_))("#\nx\n1")
  reject(p.integerLiteral(_))("#x1g")
  reject(p.integerLiteral(_))("#x1G")
  reject(p.integerLiteral(_))("#x1ö")
  reject(p.integerLiteral(_))("#b2")
  reject(p.integerLiteral(_))("#b-2")
  reject(p.integerLiteral(_))("- #b2")

  parseAst(p.processedStringLiteral(_))(""""str($foo)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str("),
    LiteralValue.StringExpressionPart(NamedValue("foo")),
    LiteralValue.StringPlainPart(")")
  )))
  parseAst(p.processedStringLiteral(_))("\" line1 \n line2\"")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" line1 \n line2")
  )))
  parseAst(p.processedStringLiteral(_))(""""str$(bar)"""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("str"),
    LiteralValue.StringExpressionPart(NamedValue("bar"))
  )))
  parseAst(p.processedStringLiteral(_))(""""$(bar)str"""")(StringLiteral(Seq(
    LiteralValue.StringExpressionPart(NamedValue("bar")),
    LiteralValue.StringPlainPart("str")
  )))
  parseAst(p.processedStringLiteral(_))(""""\"\\10\$\n"""")(StringLiteral(Seq(
    LiteralValue.StringEscapePart(Seq('"'.toString, """\""")),
    LiteralValue.StringPlainPart("10"),
    LiteralValue.StringEscapePart(Seq("$", "n"))
  )))
  reject(p.processedStringLiteral(_))(""""10$"""")
  reject(p.processedStringLiteral(_))(""""10$$"""")
  reject(p.processedStringLiteral(_))(""""10\"""")
  reject(p.processedStringLiteral(_))(""""10\ö"""")

  parseAst(p.verbatimStringLiteral(_))("' str '")(StringLiteral(Seq(
    LiteralValue.StringPlainPart(" str ")
  )))
  parseAst(p.verbatimStringLiteral(_))("'line1 \n line2'")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("line1 \n line2")
  )))
  parseAst(p.verbatimStringLiteral(_))("""'"'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart('"'.toString)
  )))
  parseAst(p.verbatimStringLiteral(_))("""'c:\n\$BAR\\a\'""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("""c:\n\$BAR\\a\""")
  )))
  parseAst(p.verbatimStringLiteral(_))("""#'''#""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'")
  )))
  parseAst(p.verbatimStringLiteral(_))("""##''#'##""")(StringLiteral(Seq(
    LiteralValue.StringPlainPart("'#")
  )))
  reject(p.verbatimStringLiteral(_))("""'''""")

  //////
  // Basic syntax

  parseAst(p.tuple(_))("((),())")(
    Tuple(Seq(UnitLiteral(), UnitLiteral()))
  )
  reject(p.tuple(_))("()")

  parseAst(p.tupleType(_))("(() , ())")(
    TupleType(Seq(UnitType(), UnitType()))
  )
  parseAst(p.tuplePattern(_))("(\n(),\n(),\n)")(
    TuplePattern(Seq(UnitPattern(), UnitPattern()))
  )


  parseAst(p.parens(_))("((()))")(UnitLiteral())
  parseAst(p.parens(_))("((\n\n  ())\n)")(UnitLiteral())
  parseAst(p.typeParens(_))("((()))")(UnitType())
  parseAst(p.patternParens(_))("((()))")(UnitPattern())

  parseAst(p.block(_))("(\n\n// comment\n()\n;\n()\n();();;)")(
    Block(Seq.fill(4)(UnitLiteral()))
  )
  parseAst(p.block(_))("(();()//\n)")(
    Block(Seq(UnitLiteral(), UnitLiteral()))
  )
  parseAst(p.block(_))("(//first\n\n\n();()//\n)")(
    Block(Seq(UnitLiteral(), UnitLiteral()))
  )
  reject(p.block(_))("(foo)")
  reject(p.block(_))("()")

  parseAst(p.namedValue(_))("foo")(NamedValue("foo"))
  parseAst(p.namedValue(_))("foo1")(NamedValue("foo1"))
  parseAst(p.namedValue(_))("_1")(NamedValue("_1"))

  parseAst(p.namedType(_))("Foo")(NamedType("Foo"))
  parseAst(p.namedType(_))("Foo<Bar>")(
    NamedType("Foo", Seq(NamedType("Bar")))
  )
  parseAst(p.namedType(_))("Foo<Bar, Baz<Zot>>")(
    NamedType("Foo", Seq(
      NamedType("Bar", Nil),
      NamedType("Baz", Seq(NamedType("Zot"))))
    )
  )

  parseAst(p.namePattern(_))("foo")(NamePattern("foo"))
  parseAst(p.namePattern(_))("foo1")(NamePattern("foo1"))
  parseAst(p.namePattern(_))("_1")(NamePattern("_1"))

  parseAst(p.valueAs(_))("foo: String")(
    ValueAs(NamedValue("foo"), NamedType("String"))
  )
  parseAst(p.valueAs(_))("(foo): (String)")(
    ValueAs(NamedValue("foo"), NamedType("String"))
  )

  parseAst(p.patternAs(_))("(): ()")(
    PatternAs(UnitPattern(), UnitType())
  )

  parseAst(p.record(_))("{}")(Record(Nil))
  parseAst(p.record(_))("{ a = foo, b: Bar = bar }")(Record(Seq(
    FieldDef("a", None, NamedValue("foo")),
    FieldDef("b", Some(NamedType("Bar")), NamedValue("bar"))
  )))
  parseAst(p.record(_))("{a, b}")(Record(Seq(
    FieldDef("a", None, NamedValue("a")),
    FieldDef("b", None, NamedValue("b"))
  )))
  parseAst(p.record(_))("{\n  a = foo\n  b: Bar = bar\n}")(Record(Seq(
    FieldDef("a", None, NamedValue("foo")),
    FieldDef("b", Some(NamedType("Bar")), NamedValue("bar"))
  )))

  parseAst(p.recordType(_))("{}")(RecordType(Nil))
  parseAst(p.recordType(_))("{ a: Foo, b: Bar }")(RecordType(Seq(
    FieldDecl("a", NamedType("Foo")),
    FieldDecl("b", NamedType("Bar"))
  )))

  parseAst(p.unionType(_))("A | {} | ()")(
    UnionType(Seq(NamedType("A"), RecordType(Nil), UnitType()))
  )

  parseAst(p.intersectionType(_))("A & {} & ()")(
    IntersectionType(Seq(NamedType("A"), RecordType(Nil), UnitType()))
  )

  parseAst(p.lambda(_))("() => ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parseAst(p.lambda(_))("a => a")(
    Lambda(NamePattern("a"), NamedValue("a"))
  )
  parseAst(p.lambda(_))("<A>() => ()")(
    Lambda(Seq(TypeParam("A")), UnitPattern(), UnitLiteral())
  )
  parseAst(p.lambda(_))("() =>\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  // TODO accept or reject?
  parseAst(p.lambda(_))("() =>\n\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  parseAst(p.lambda(_))("() =>//comment\n  ()")(
    Lambda(UnitPattern(), UnitLiteral())
  )
  reject(p.lambda(_))("()\n=> ()")

  parseAst(p.piecewise(_))("#(() => ())")(
    Piecewise(Seq(UnitPattern() -> UnitLiteral()))
  )
  parseAst(p.piecewise(_))("#(\n  () =>\n    ()\n)")(
    Piecewise(Seq(UnitPattern() -> UnitLiteral()))
  )
  parseAst(p.piecewise(_))("#(A => a, B => b)")(
    Piecewise(Seq(NamePattern("A") -> NamedValue("a"), NamePattern("B") -> NamedValue("b")))
  )
  parseAst(p.piecewise(_))("#(\n  A => a\n  B => b\n)")(
    Piecewise(Seq(NamePattern("A") -> NamedValue("a"), NamePattern("B") -> NamedValue("b")))
  )
  parseAst(p.piecewise(_))("#(\n  A => a,\n  B => b,\n)")(
    Piecewise(Seq(NamePattern("A") -> NamedValue("a"), NamePattern("B") -> NamedValue("b")))
  )
  parseAst(p.piecewise(_))("#()")(
    Piecewise(Nil)
  )

  parseAst(p.funcType(_))("() => ()")(
    FuncType(UnitType(), UnitType())
  )
  parseAst(p.funcType(_))("<A>() => ()")(
    FuncType(Seq(TypeParam("A")), UnitType(), UnitType())
  )

  parseAst(p.memberOrJuxtaCall(_))("foo.length.toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedValue("length")), NamedValue("toString"))
  )
  parseAst(p.memberOrJuxtaCall(_))("{}.a")(
    MemberSelection(Record(Nil), NamedValue("a"))
  )
  parseAst(p.memberOrJuxtaCall(_))("foo()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parseAst(p.memberOrJuxtaCall(_))("foo.bar(zot.baz)(buz(qux))"){
    val foobar = MemberSelection(NamedValue("foo"), NamedValue("bar"))
    val zotbaz = MemberSelection(NamedValue("zot"), NamedValue("baz"))
    val buzqux = Call(NamedValue("buz"), NamedValue("qux"))

    Call(Call(foobar, zotbaz), buzqux)
  }
  parseAst(p.memberOrJuxtaCall(_))("foo.\n  length.\n  toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedValue("length")), NamedValue("toString"))
  )
  parseAst(p.memberOrJuxtaCall(_))("foo\n  .length\n  .toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedValue("length")), NamedValue("toString"))
  )
  parseAst(p.memberOrJuxtaCall(_))("foo/**/\n  ./**/length/**/./**/toString")(
    MemberSelection(MemberSelection(NamedValue("foo"), NamedValue("length")), NamedValue("toString"))
  )
  reject(p.memberOrJuxtaCall(_))("foo ()")
  parseAst(p.spaceCall(_))("foo ()")(
    Call(NamedValue("foo"), UnitLiteral())
  )
  parseAst(p.spaceCall(_))("foo bar")(
    Call(NamedValue("foo"), NamedValue("bar"))
  )
  parseAst(p.spaceCall(_))("foo bar baz")(
    Call(Call(NamedValue("foo"), NamedValue("bar")), NamedValue("baz"))
  )
  parseAst(p.spaceCall(_))("foo bar.baz")(
    Call(NamedValue("foo"), MemberSelection(NamedValue("bar"), NamedValue("baz")))
  )
  parseAst(p.spaceCall(_))("foo .bar baz")(
    Call(MemberSelection(NamedValue("foo"), NamedValue("bar")), NamedValue("baz"))
  )
  parseAst(p.spaceCall(_))("foo.bar zot.baz buz(qux)"){
    val foobar = MemberSelection(NamedValue("foo"), NamedValue("bar"))
    val zotbaz = MemberSelection(NamedValue("zot"), NamedValue("baz"))
    val buzqux = Call(NamedValue("buz"), NamedValue("qux"))

    Call(Call(foobar, zotbaz), buzqux)
  }
  parseAst(p.spaceCall(_))("foo.bar zot/**/(baz) buz(qux)"){
    val foobar = MemberSelection(NamedValue("foo"), NamedValue("bar"))
    val zotbaz = Call(NamedValue("zot"), NamedValue("baz"))
    val buzqux = Call(NamedValue("buz"), NamedValue("qux"))

    Call(Call(foobar, zotbaz), buzqux)
  }
  parseAst(p.pipe(_))("bar @ foo")(
    Pipe(NamedValue("bar"), NamedValue("foo"))
  )
  parseAst(p.pipe(_))("bar @ foo @ baz")(
    Pipe(Pipe(NamedValue("bar"), NamedValue("foo")), NamedValue("baz"))
  )
  parseAst(p.pipe(_))("foo @ bar.zot baz")(
    Pipe(NamedValue("foo"), Call(MemberSelection(NamedValue("bar"), NamedValue("zot")), NamedValue("baz")))
  )
  parseAst(p.pipe(_))("foo bar.zot @ baz")(
    Pipe(Call(NamedValue("foo"), MemberSelection(NamedValue("bar"), NamedValue("zot"))), NamedValue("baz"))
  )
  parseAst(p.pipe(_))("foo\n  .bar\n  @ bas baz.qux\n  @ zot()\n  .noz"){
    val foobar = MemberSelection(NamedValue("foo"), NamedValue("bar"))
    val bazqux = MemberSelection(NamedValue("baz"), NamedValue("qux"))
    val zotnoz = MemberSelection(Call(NamedValue("zot"), UnitLiteral()), NamedValue("noz"))

    Pipe(Pipe(foobar, Call(NamedValue("bas"), bazqux)), zotnoz)
  }

  //////
  // Declarations

  parseAst(p.attributes(_))("@[a b]@[c]")(Seq(Attribute("a", Some("b")), Attribute("c", None)))
  parseAst(p.attributes(_))("@[a]\n@[c]\n")(Seq(Attribute("a", None), Attribute("c", None)))

  parseAst(p.typeDef(_))("::alias Foo: Bar")(
    TypeDef(TypeDefKind.Alias, "Foo", NamedType("Bar"))
  )

  parseAst(p.typeDef(_))("::alias Foo<A, B>: Bar<B, A>"){
    val bar = NamedType("Bar", Seq(NamedType("B"), NamedType("A")))

    TypeDef(TypeDefKind.Alias, "Foo", Seq(TypeParam("A"), TypeParam("B")), bar)
  }

  parseAst(p.typeDef(_))("::tagged Foo: Bar")(
    TypeDef(TypeDefKind.Tagged, "Foo", NamedType("Bar"))
  )

  parseAst(p.typeDef(_))("::tagged Foo<A, B>: (A, B)"){
    val abTuple = TupleType(Seq(NamedType("A"), NamedType("B")))

    TypeDef(TypeDefKind.Tagged, "Foo", Seq(TypeParam("A"), TypeParam("B")), abTuple)
  }

  parseAst(p.typeDef(_))("::boxed Foo: Bar")(
    TypeDef(TypeDefKind.Boxed, "Foo", NamedType("Bar"))
  )

  parseAst(p.typeDef(_))("::boxed Foo<A, B>:\n  {a: A, b: B}"){
    val abRecord = RecordType(Seq(FieldDecl("a", NamedType("A")), FieldDecl("b", NamedType("B"))))

    TypeDef(TypeDefKind.Boxed, "Foo", Seq(TypeParam("A"), TypeParam("B")), abRecord)
  }

  parseAst(p.typeDef(_))("::opaque Foo")(
    TypeDef(TypeDefKind.Opaque, "Foo")
  )

  parseAst(p.typeDef(_))("::opaque Foo<Nonsense>: Nonsense")(
    TypeDef(TypeDefKind.Opaque, "Foo", Seq(TypeParam("Nonsense")), NamedType("Nonsense"))
  )

  parseAst(p.typeDef(_))("::singleton Foo")(
    TypeDef(TypeDefKind.Singleton, "Foo")
  )

  parseAst(p.typeDef(_))("::singleton Foo<Nonsense>: Nonsense")(
    TypeDef(TypeDefKind.Singleton, "Foo", Seq(TypeParam("Nonsense")), NamedType("Nonsense"))
  )

  parseAst(p.methodDeclSection(_))("::declare ::methods Bar: {\n  foo: Bar\n}")(
    MethodDeclSection(NameWithPos("Bar"), Seq(
      MethodDecl(Nil, "foo", NamedType("Bar", Nil))
    ))
  )
  parseAst(p.methodDeclSection(_))("::declare ::methods Bar: { foo: Bar = sic }")(
    MethodDeclSection(NameWithPos("Bar"), Seq(
      MethodDef(Nil, "foo", Some(NamedType("Bar", Nil)), NamedValue("sic"))
    ))
  )
  parseAst(p.methodDeclSection(_))("::declare ::methods Bar: {\n@[a]\nfoo: Bar\nbaz: Baz\n}")(
    MethodDeclSection(NameWithPos("Bar"), Seq(
      MethodDecl(Seq(Attribute("a", None)), "foo", NamedType("Bar", Nil)),
      MethodDecl(Nil, "baz", NamedType("Baz", Nil)),
    ))
  )

  parseAst(p.methodDefSection(_))("::methods Bar b: { foo = b }")(
    MethodDefSection(NameWithPos("Bar"), NamePattern("b"), Seq(
      MethodDef(Nil, "foo", None, NamedValue("b"))
    ))
  )
  parseAst(p.methodDefSection(_))("::methods Bar b:\n  {foo: Bar = b}")(
    MethodDefSection(NameWithPos("Bar"), NamePattern("b"), Seq(
      MethodDef(Nil, "foo", Some(NamedType("Bar", Nil)), NamedValue("b"))
    ))
  )
  parseAst(p.methodDefSection(_))("::methods Bar b: { foo: Sic }")(
    MethodDefSection(NameWithPos("Bar"), NamePattern("b"), Seq(
      MethodDecl(Nil, "foo", NamedType("Sic", Nil))
    ))
  )
  // FIXME
  // parseAst(p.methodDefSection(_))(
  //   "::methods Box a: { map: <B>(A => B) => Box<B> = <B>(f: A => B) => Box{ x = f a.x } }"
  // )(
  //   MethodDefSection(TypeName("Box"), NamePattern("a"), Seq(
  //     MethodDef(
  //       Nil,
  //       "map",
  //       FuncType(),
  //       Lambda(
  //         PatternAs(NamePattern("f"), FuncType()),

  //       )
  //     )
  //   ))
  // )

  parseAst(p.valueDecl(_))("::declare foo: ()")(ValueDecl("foo", UnitType()))

  parseAst(p.valueDef(_))("a =\u0020\u0020\n\u0020\u0020\n  foo")(
    ValueDef(NamePattern("a"), NamedValue("foo"))
  )
  parseAst(p.valueDef(_))("(a, b) = c")(
    ValueDef(TuplePattern(Seq(NamePattern("a"), NamePattern("b"))), NamedValue("c"))
  )

  //////
  // Top-level

  parseAst(p.programBlock(_))("(foo/*comment\n /*  \n // line2*/)")(
    Block(Seq(NamedValue("foo")))
  )
  parseAst(p.programBlock(_))("foo//\n")(
    Block(Seq(NamedValue("foo")))
  )
  parseAst(p.programBlock(_))("foo//")(
    Block(Seq(NamedValue("foo")))
  )
  parseAst(p.programBlock(_))("//first\n\n()")(
    Block(Seq(UnitLiteral()))
  )

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

  parseAst(p.valueExpr(_))("1234")(IntegerLiteral(LiteralValue.IntegerDec("1234")))
  parseAst(p.valueExpr(_))("(a = (); a)")(Block(Seq(ValueDef(NamePattern("a"), UnitLiteral()), NamedValue("a"))))
  parseAst(p.valueExpr(_))("(a => a)")(Lambda(NamePattern("a"), NamedValue("a")))
  reject(p.valueExpr(_))("foo square = 1")
  reject(p.valueExpr(_))("square = 1")
}
