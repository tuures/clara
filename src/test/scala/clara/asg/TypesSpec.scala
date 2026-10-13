package clara.asg

import clara.ast.NoPos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe.SafeStringContext

import clara.testutil.BaseSpec

class TypesSpec extends BaseSpec {
  import TypeCons._
  import Types._

  def testSimplification(t1: Type, t2: Type, description: String): Unit = {
    test(safe"simplification: $description") {
      assert(t1 === t2)
    }
  }

  def testHeadNormalization(t1: Type, t2: Type, description: String): Unit = {
    test(safe"head normalization: $description") {
      assert(normalizeHead(t1) === t2)
    }
  }

  testSimplification(Union(Nil), Bottom, "empty union = Bottom")
  testSimplification(Intersection(Nil), Top, "empty intersection = Top")

  testSimplification(
    Union(Seq(Uni, Uni)), Union(Seq(Uni)),
    "indentical representations removed: () | () = ()"
  )
  testSimplification(
    Intersection(Seq(Uni, Uni)), Intersection(Seq(Uni)),
    "indentical representations removed: () & () = ()"
  )

  val aParamCon = ParamCon("A", NoPos)
  val aParamCon2 = ParamCon("A", NoPos)
  val bParamCon = ParamCon("B", NoPos)
  val bParamCon2 = ParamCon("B", NoPos)
  val cParamCon = ParamCon("C", NoPos)

  val aParam = Param(aParamCon)
  val aParam2 = Param(aParamCon2)
  val bParam = Param(bParamCon)
  val bParam2 = Param(bParamCon2)
  val cParam = Param(cParamCon)

  testHeadNormalization(
    Intersection(Seq(Union(Seq(aParam, bParam)), Uni)),
    Union(Seq(Intersection(Seq(aParam, Uni)), Intersection(Seq(bParam, Uni)))),
    "intersection distributes over unions"
  )
  testHeadNormalization(
    Intersection(Seq(Record("a" -> Uni), Record("b" -> Top))),
    Record("a" -> Uni, "b" -> Top),
    "record intersections merge fields"
  )
  testHeadNormalization(
    Intersection(Seq(Record("a" -> Top), Record("a" -> Uni))),
    Record("a" -> Uni),
    "shared record fields are intersected"
  )
  testHeadNormalization(
    Intersection(Seq(Record("a" -> Uni), Record("a" -> Bottom))),
    Bottom,
    "record intersections with conflicting fields reduce to Bottom"
  )

  val funcAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "FunctionAlias",
    Seq(aParam.con, bParam.con),
    Func(aParam, bParam),
    NoPos,
  )
  val funcUniUniAliasType = Alias(funcAliasCon, Seq(Uni, Uni), Func(Uni, Uni))

  val justTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "JustTagged", Seq(aParam.con), aParam, NoPos)
  val justUniTaggedType = Tagged(justTaggedCon, Seq(Uni), Uni)
  val justTopTaggedType = Tagged(justTaggedCon, Seq(Top), Top)

  val justBoxedCon = WrapperTypeCon(TypeDefKind.Boxed, "JustBoxed", Seq(aParam.con), aParam, NoPos)
  val justUniBoxedType = Boxed(justBoxedCon, Seq(Uni), Uni)
  val justTopBoxedType = Boxed(justBoxedCon, Seq(Top), Top)

  val fooOpaqueCon = OpaqueTypeCon("FooOpaque", Seq(aParam.con), NoPos)
  val fooUniOpaqueType = Opaque(fooOpaqueCon, Seq(Uni))
  val fooTopOpaqueType = Opaque(fooOpaqueCon, Seq(Top))

  val barOpaqueCon = OpaqueTypeCon("BarOpaque", Seq(aParam.con), NoPos)
  val barUniOpaqueType = Opaque(barOpaqueCon, Seq(Uni))


  val funcUniUniOpaqueType = Opaque(fooOpaqueCon, Seq(Func(Uni, Uni)))
  val funcUniUniAliasOpaqueType = Opaque(fooOpaqueCon, Seq(funcUniUniAliasType))

  val blueSingleton = Singleton(SingletonTypeCon("BlueSingleton", NoPos))
  val redSingleton = Singleton(SingletonTypeCon("RedSingleton", NoPos))
  val unusedTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "UnusedTagged", Seq(aParam.con), Uni, NoPos)
  val handlerTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "Handler", Seq(aParam.con), Func(aParam, Uni), NoPos)
  val unusedBoxedCon = WrapperTypeCon(TypeDefKind.Boxed, "UnusedBoxed", Seq(aParam.con), Uni, NoPos)

  testHeadNormalization(
    Intersection(Seq(blueSingleton, redSingleton)),
    Bottom,
    "disjoint singleton intersections reduce to Bottom"
  )

  val redBlueUnionAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "RedOrBlueAlias",
    Nil,
    Union(Seq(redSingleton, blueSingleton)),
    NoPos,
  )
  val redBlueUnionAliasType = Alias(redBlueUnionAliasCon, Nil, Union(Seq(redSingleton, blueSingleton)))
  val greenSingleton = Singleton(SingletonTypeCon("GreenSingleton", NoPos))

  testHeadNormalization(
    Intersection(Seq(redBlueUnionAliasType, greenSingleton)),
    Bottom,
    "intersection with a disjoint alias of a union reduces to Bottom"
  )
  testSimplification(
    Intersection(Seq(redSingleton, blueSingleton)),
    new Intersection(Seq(redSingleton, blueSingleton)),
    "construction keeps an intersection as written even when it's empty"
  )
  testSimplification(
    Intersection(Seq(Union(Seq(aParam, bParam)), Uni)),
    new Intersection(Seq(new Union(Seq(aParam, bParam)), Uni)),
    "construction keeps unions inside intersections as written, only normalizeHead distributes them"
  )
  testHeadNormalization(
    Intersection(Seq(redBlueUnionAliasType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "intersection distributes over an alias of a union"
  )
  testAssignable(true)(
    Intersection(Seq(redBlueUnionAliasType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "RedOrBlueAlias & C, (Red & C) | (Blue & C)"
  )
  testAssignable(true)(
    Intersection(Seq(redSingleton, blueSingleton)), aParam,
    "Red & Blue, A (empty intersection is assignable to anything)"
  )
  testAssignable(false)(
    Uni, Intersection(Seq(redSingleton, blueSingleton)),
    "(), Red & Blue"
  )
  def testDisjoint(expected: Boolean)(t1: Type, t2: Type, description: String): Unit = {
    test(safe"${if (expected) "" else "!"}disjoint($description)") {
      assert(disjoint(t1, t2) === expected)
      assert(disjoint(t2, t1) === expected, "disjoint should be symmetric")
    }
  }

  val redGreenTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "RedGreenTagged", Nil, Union(Seq(redSingleton, greenSingleton)), NoPos)
  val redGreenTaggedType = Tagged(redGreenTaggedCon, Nil, Union(Seq(redSingleton, greenSingleton)))

  testDisjoint(true)(Bottom, aParam, "!, A")
  testDisjoint(true)(redSingleton, blueSingleton, "Red, Blue")
  testDisjoint(false)(redSingleton, redSingleton, "Red, Red")
  testDisjoint(true)(redBlueUnionAliasType, greenSingleton, "RedOrBlueAlias, Green")
  testDisjoint(false)(redBlueUnionAliasType, redSingleton, "RedOrBlueAlias, Red")
  testDisjoint(true)(Intersection(Seq(aParam, redSingleton)), blueSingleton, "A & Red, Blue")
  testDisjoint(false)(redGreenTaggedType, Union(Seq(redSingleton, blueSingleton)), "RedGreenTagged, Red | Blue")
  testDisjoint(true)(redGreenTaggedType, blueSingleton, "RedGreenTagged, Blue")
  testDisjoint(true)(Tuple(Seq(Uni)), Tuple(Seq(Uni, Uni)), "((),), ((), ())")
  testDisjoint(true)(Tuple(Seq(redSingleton, Uni)), Tuple(Seq(blueSingleton, Uni)), "(Red, ()), (Blue, ())")
  testDisjoint(false)(Tuple(Seq(redSingleton, aParam)), Tuple(Seq(redSingleton, Uni)), "(Red, A), (Red, ())")
  testDisjoint(true)(Record("a" -> redSingleton), Record("a" -> blueSingleton), "{a: Red}, {a: Blue}")
  testDisjoint(true)(
    Record("a" -> redSingleton, "b" -> Uni), Record("a" -> blueSingleton), "{a: Red, b: ()}, {a: Blue}"
  )
  testDisjoint(false)(Record("a" -> redSingleton), Record("a" -> redBlueUnionAliasType), "{a: Red}, {a: RedOrBlueAlias}")
  testDisjoint(false)(Record("a" -> redSingleton), Record("b" -> blueSingleton), "{a: Red}, {b: Blue}")
  testDisjoint(true)(justUniBoxedType, Boxed(unusedBoxedCon, Seq(Uni), Uni), "JustBoxed<()>, UnusedBoxed<()>")
  testDisjoint(true)(
    Boxed(justBoxedCon, Seq(redSingleton), redSingleton), Boxed(justBoxedCon, Seq(blueSingleton), blueSingleton),
    "JustBoxed<Red>, JustBoxed<Blue>"
  )
  testDisjoint(true)(justUniBoxedType, redSingleton, "JustBoxed<()>, Red")
  testDisjoint(true)(fooUniOpaqueType, barUniOpaqueType, "FooOpaque<()>, BarOpaque<()>")
  testDisjoint(false)(fooUniOpaqueType, Opaque(fooOpaqueCon, Seq(Top)), "FooOpaque<()>, FooOpaque<*> (same opaque type)")
  testDisjoint(true)(fooUniOpaqueType, redSingleton, "FooOpaque<()>, Red")
  testDisjoint(true)(fooUniOpaqueType, justUniBoxedType, "FooOpaque<()>, JustBoxed<()>")
  // conservative: overlap can't be ruled out
  testDisjoint(false)(aParam, redSingleton, "A, Red")
  testDisjoint(false)(aParam, bParam, "A, B")
  testDisjoint(false)(fooUniOpaqueType, Record("a" -> Uni), "FooOpaque<()>, {a: ()}")

  def testAssignable(expected: Boolean)(t1: Type, t2: Type, description: String = ""): Unit = {
    val desc = if (description.length > 0) description else safe"${t1.toString()}, ${t2.toString()}"
    test(safe"${if(expected) "" else "!"}isAssignable($desc)") {
      assert(isAssignable(t1, t2) === expected)
    }
  }

  testAssignable(true)(Top, Top)
  testAssignable(true)(Bottom, Bottom)
  testAssignable(false)(Top, Bottom)
  testAssignable(true)(Bottom, Top)

  testAssignable(true)(Uni, Uni)
  testAssignable(true)(Uni, Top)
  testAssignable(false)(Top, Uni)
  testAssignable(false)(Uni, Bottom)
  testAssignable(true)(Bottom, Uni)

  testAssignable(true)(
    Intersection(Seq(aParam, bParam)), Intersection(Seq(aParam, bParam)),
    "A & B, A & B"
  )
  testAssignable(true)(
    Intersection(Seq(aParam, bParam, cParam)), Intersection(Seq(aParam, bParam)),
    "A & B & C, A & B"
  )
  testAssignable(true)(
    Intersection(Seq(aParam, bParam)), Union(Seq(Intersection(Seq(aParam, bParam)), Uni)),
    "A & B, (A & B) | ()"
  )
  testAssignable(true)(
    Intersection(Seq(Union(Seq(aParam, bParam)), Uni)),
    Union(Seq(Intersection(Seq(aParam, Uni)), Intersection(Seq(bParam, Uni)))),
    "(A | B) & (), (A & ()) | (B & ())"
  )
  testAssignable(true)(
    Intersection(Seq(Record("a" -> Uni), Record("b" -> Uni))),
    Record("a" -> Uni, "b" -> Uni),
    "{a: ()} & {b: ()}, {a: (), b: ()}"
  )

  testAssignable(true)(Func(Uni, Uni), Func(Uni, Uni), "() => (), () => ()")
  testAssignable(true)(Func(Uni, Uni), Func(Uni, Top), "() => (), () => *")
  testAssignable(false)(Func(Uni, Uni), Func(Top, Uni), "() => (), * => ()")
  testAssignable(true)(Func(Top, Uni), Func(Uni, Uni), "* => (), () => ()")
  testAssignable(false)(Func(Uni, Top), Func(Uni, Uni), "() => *, () => ()")

  testAssignable(true)(
    Func(Seq(aParamCon), aParam, Uni), Func(Seq(aParamCon2), aParam2, Uni),
    "<A> A => (), <A> A => ()"
  )
  testAssignable(true)(
    Func(Seq(aParamCon), aParam, Uni), Func(Seq(aParamCon2), aParam2, Top),
    "<A> A => (), <A> A => *"
  )

  testAssignable(true)(
    Func(Seq(aParamCon), aParam, Uni), Func(Seq(bParamCon), bParam, Uni),
    "<A> A => (), <B> B => ()"
  )
  testAssignable(true)(
    Func(Seq(aParamCon), aParam, aParam), Func(Seq(bParamCon), bParam, bParam),
    "<A> A => A, <B> B => B"
  )

  testAssignable(false)(
    Func(Seq(aParamCon, bParamCon), aParam, Uni), Func(Seq(aParamCon), aParam, Uni),
    "<A, B> A => (), <A> A => ()"
  )
  testAssignable(false)(
    Func(Seq(aParamCon), aParam, Uni), Func(Seq(aParamCon, bParamCon), aParam, Uni),
    "<A> A => (), <A, B> A => ()"
  )

  testAssignable(true)(Record(), Record(), "{} , {}")
  testAssignable(true)(Record("foo" -> Uni), Record("foo" -> Uni), "{foo: ()}, {foo: ()}")
  testAssignable(true)(Record("foo" -> Uni), Record("foo" -> Top), "{foo: ()}, {foo: *}")
  testAssignable(false)(Record("foo" -> Top), Record("foo" -> Uni), "{foo: *}, {foo: ()}")
  testAssignable(false)(Record("foo" -> Uni), Record("bar" -> Uni), "{foo: ()}, {bar: ()}")
  testAssignable(true)(Record("foo" -> Uni, "zot" -> Uni), Record("foo" -> Uni), "{foo: (), zot: ()}, {foo: ()}")
  testAssignable(false)(Record("foo" -> Uni), Record("foo" -> Uni, "zot" -> Uni), "{foo: ()}, {foo: (), zot: ()}")

  testAssignable(true)(Tuple(Seq(Uni, Uni)), Tuple(Seq(Uni, Uni)), "((), ()), ((), ())")
  testAssignable(true)(
    Tuple(Seq(Uni, Record("foo" -> Uni, "zot" -> Uni))), Tuple(Seq(Uni, Record("foo" -> Uni))),
    "((), {foo: (), zot: ()}), ((), {foo: ()})"
  )
  testAssignable(false)(Tuple(Seq(Uni, Uni)), Tuple(Seq(Uni, Record("foo" -> Uni))), "((), ()), ((), {foo: ()})")

  testAssignable(true)(
    Union(Seq(Uni, Record("foo" -> Uni))), Union(Seq(Uni, Record("foo" -> Uni))),
    "(() | {foo: ()}), (() | {foo: ()})"
  )
  testAssignable(true)(
    Union(Seq(Record("foo" -> Uni), Uni)), Union(Seq(Uni, Record("foo" -> Uni))),
    "({foo: ()} | ()), (() | {foo: ()})"
  )
  testAssignable(true)(
    Union(Seq(Record("foo" -> Uni), Uni)), Union(Seq(Uni, Record())),
    "({foo: ()} | ()), (() | {})"
  )

  testAssignable(true)(aParam, aParam)
  testAssignable(false)(aParam, bParam)
  testAssignable(false)(aParam, aParam2)

  testAssignable(true)(funcUniUniAliasType, funcUniUniAliasType, "FunctionAlias<Uni, Uni>, FunctionAlias<Uni, Uni>")
  testAssignable(true)(Func(Uni, Uni), funcUniUniAliasType, "Func(Uni, Uni), FunctionAlias<Uni, Uni>")
  testAssignable(true)(funcUniUniAliasType, Func(Uni, Uni), "FunctionAlias<Uni, Uni>, Func(Uni, Uni)")
  testAssignable(true)(funcUniUniAliasType, Func(Uni, Top), "FunctionAlias<Uni, Uni>, Func(Uni, Top)")
  testAssignable(false)(funcUniUniAliasType, Func(Top, Uni), "FunctionAlias<Uni, Uni>, Func(Top, Uni)")
  testAssignable(true)(Func(Top, Uni), funcUniUniAliasType, "Func(Top, Uni), FunctionAlias<Uni, Uni>")
  testAssignable(false)(Func(Uni, Top), funcUniUniAliasType, "Func(Uni, Top), FunctionAlias<Uni, Uni>")

  testAssignable(true)(justUniTaggedType, justUniTaggedType, "JustTagged<Uni>, JustTagged<Uni>")
  testAssignable(true)(justUniTaggedType, justTopTaggedType, "JustTagged<Uni>, JustTagged<Top>")
  testAssignable(false)(justTopTaggedType, justUniTaggedType, "JustTagged<Top>, JustTagged<Uni>")
  testAssignable(false)(
    Tagged(unusedTaggedCon, Seq(aParam), Uni), Tagged(unusedTaggedCon, Seq(bParam), Uni),
    "UnusedTagged<A>, UnusedTagged<B>"
  )
  testAssignable(true)(
    Tagged(handlerTaggedCon, Seq(Top), Func(Top, Uni)), Tagged(handlerTaggedCon, Seq(Uni), Func(Uni, Uni)),
    "Handler<*>, Handler<()> (contravariant)"
  )
  testAssignable(false)(
    Tagged(handlerTaggedCon, Seq(Uni), Func(Uni, Uni)), Tagged(handlerTaggedCon, Seq(Top), Func(Top, Uni)),
    "Handler<()>, Handler<*> (contravariant)"
  )
  testAssignable(true)(justUniTaggedType, Uni, "JustTagged<Uni>, Uni")
  testAssignable(false)(Uni, justUniTaggedType, "Uni, JustTagged<Uni>")

  testAssignable(true)(justUniBoxedType, justUniBoxedType, "JustBoxed<Uni>, JustBoxed<Uni>")
  testAssignable(true)(justUniBoxedType, justTopBoxedType, "JustBoxed<Uni>, JustBoxed<Top>")
  testAssignable(false)(justTopBoxedType, justUniBoxedType, "JustBoxed<Top>, JustBoxed<Uni>")
  testAssignable(false)(
    Boxed(unusedBoxedCon, Seq(aParam), Uni), Boxed(unusedBoxedCon, Seq(bParam), Uni),
    "UnusedBoxed<A>, UnusedBoxed<B>"
  )
  testAssignable(false)(justUniBoxedType, Uni, "JustBoxed<Uni>, Uni")
  testAssignable(false)(Uni, justUniBoxedType, "Uni, JustBoxed<Uni>")

  testAssignable(true)(fooUniOpaqueType, fooUniOpaqueType, "FooOpaque<Uni>, FooOpaque<Uni>")
  testAssignable(false)(fooUniOpaqueType, fooTopOpaqueType, "FooOpaque<Uni>, FooOpaque<Top>")
  testAssignable(false)(fooTopOpaqueType, fooUniOpaqueType, "FooOpaque<Top>, FooOpaque<Uni>")
  testAssignable(false)(fooUniOpaqueType, Uni, "FooOpaque<Uni>, Uni")
  testAssignable(false)(Uni, fooUniOpaqueType, "Uni, FooOpaque<Uni>")

  testAssignable(false)(fooUniOpaqueType, barUniOpaqueType, "FooOpaque<Uni>, BarOpaque<Uni>")
  testAssignable(false)(barUniOpaqueType, fooUniOpaqueType, "BarOpaque<Uni>, FooOpaque<Uni>")

  testAssignable(true)(
    funcUniUniOpaqueType, funcUniUniAliasOpaqueType,
    "FooOpaque<Uni => Uni>, FooOpaque<FunctionAlias<Uni, Uni>>"
  )
  testAssignable(true)(
    funcUniUniAliasOpaqueType, funcUniUniOpaqueType,
    "FooOpaque<FunctionAlias<Uni, Uni>>, FooOpaque<Uni => Uni>"
  )

  testAssignable(true)(blueSingleton, blueSingleton, "BlueSingleton, BlueSingleton")
  testAssignable(false)(blueSingleton, redSingleton, "BlueSingleton, RedSingleton")
  testAssignable(false)(redSingleton, blueSingleton, "RedSingleton, BlueSingleton")

  testAssignable(true)(
    funcUniUniAliasType, Union(Seq(Func(Uni, Uni), Uni)),
    "FunctionAlias<Uni, Uni>, (Uni => Uni) | Uni"
  )
  testAssignable(true)(
    Intersection(Seq(Func(Uni, Uni), Uni)), funcUniUniAliasType,
    "(Uni => Uni) | Uni, FunctionAlias<Uni, Uni>"
  )
  testAssignable(true)(
    redBlueUnionAliasType, Union(Seq(redSingleton, blueSingleton)),
    "RedOrBlueAlias, RedSingleton | BlueSingleton"
  )
  testAssignable(true)(
    Intersection(Seq(redSingleton, blueSingleton)), redBlueUnionAliasType,
    "RedSingleton | BlueSingleton, RedOrBlueAlias"
  )

  test("findSubstitutions finds parameters in nested structural types") {
    val parameterType = Tuple(Seq(
      aParam,
      Record("value" -> bParam),
      Func(aParam, bParam),
    ))
    val argumentType = Tuple(Seq(
      Uni,
      Record("value" -> Top),
      Func(Uni, Top),
    ))

    assert(findSubstitutions(Set(aParamCon.uniq, bParamCon.uniq), parameterType, argumentType) === Map(
      aParamCon.uniq -> Uni,
      bParamCon.uniq -> Top,
    ))
  }

  test("findSubstitutions groups repeated occurrences of a parameter") {
    assert(findSubstitutions(Set(aParamCon.uniq), Tuple(Seq(aParam, aParam)), Tuple(Seq(Uni, Uni))) === Map(
      aParamCon.uniq -> Uni,
    ))
  }

  test("findSubstitutions does not infer parameters outside the requested set") {
    assert(findSubstitutions(Set(aParamCon.uniq), Tuple(Seq(aParam, bParam)), Tuple(Seq(Uni, Top))) === Map(
      aParamCon.uniq -> Uni,
    ))
  }

  test("substituteParams recursively substitutes structural and nominal types") {
    val substitutions = Map[Uniq, Type](
      aParamCon.uniq -> Uni,
      bParamCon.uniq -> Top,
    )
    val typ = Func(
      Seq(aParamCon2, bParamCon2),
      Tuple(Seq(
        aParam,
        Record("value" -> bParam),
        Union(Seq(aParam, bParam)),
        Alias(funcAliasCon, Seq(aParam, bParam), Func(aParam, bParam)),
        Tagged(justTaggedCon, Seq(aParam), aParam),
        Boxed(justBoxedCon, Seq(bParam), bParam),
        Opaque(fooOpaqueCon, Seq(aParam)),
        aParam2,
        bParam2,
      )),
      Intersection(Seq(aParam, bParam)),
    )
    val expected = Func(
      Seq(aParamCon2, bParamCon2),
      Tuple(Seq(
        Uni,
        Record("value" -> Top),
        Union(Seq(Uni, Top)),
        Alias(funcAliasCon, Seq(Uni, Top), Func(Uni, Top)),
        Tagged(justTaggedCon, Seq(Uni), Uni),
        Boxed(justBoxedCon, Seq(Top), Top),
        Opaque(fooOpaqueCon, Seq(Uni)),
        aParam2,
        bParam2,
      )),
      Intersection(Seq(Uni, Top)),
    )

    assert(substituteParams(substitutions, typ) === expected)
  }

  def testToSource(con: TypeCon)(expected: String) = {
    test(safe"toSource(${con.toString()}) $expected") {
      assert(TypeCons.toSource(con) === expected)
    }
  }

  def testToSource(typ: Type)(expected: String) = {
    test(safe"toSource(${typ.toString()}) $expected") {
      assert(Types.toSource(typ) === expected)
    }
  }

  testToSource(Top)("*")
  testToSource(Bottom)("!")
  testToSource(Uni)("()")
  testToSource(Func(Uni, Uni))("() => ()")
  testToSource(Record())("{}")
  testToSource(Record("a" -> Uni, "b" -> Func(Uni, Uni)))("{a: (), b: () => ()}")
  testToSource(ParamCon("A", NoPos))("A")
  testToSource(Param(ParamCon("A", NoPos)))("A")
  testToSource(WrapperTypeCon(TypeDefKind.Alias, "Unit", Nil, Uni, NoPos))("Unit")
  testToSource(Alias(WrapperTypeCon(TypeDefKind.Alias, "Unit", Nil, Uni, NoPos), Nil, Uni))("Unit")
  testToSource(funcUniUniAliasType.con)("FunctionAlias<A, B>")
  testToSource(funcUniUniAliasType)("FunctionAlias<(), ()>")

  // unions nested in tuples, records and tagged types are expanded when the target is a union

  val redOrBlue = Union(Seq(redSingleton, blueSingleton))
  val redOrBlueTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "RedOrBlueTagged", Nil, redOrBlue, NoPos)
  val redOrBlueTaggedType = Tagged(redOrBlueTaggedCon, Nil, redOrBlue)

  testAssignable(true)(
    Tuple(Seq(redOrBlue, Uni)), Union(Seq(Tuple(Seq(redSingleton, Uni)), Tuple(Seq(blueSingleton, Uni)))),
    "(Red | Blue, ()), (Red, ()) | (Blue, ())"
  )
  testAssignable(false)(
    Tuple(Seq(redOrBlue, Uni)), Union(Seq(Tuple(Seq(redSingleton, Uni)), Tuple(Seq(greenSingleton, Uni)))),
    "(Red | Blue, ()), (Red, ()) | (Green, ())"
  )
  testAssignable(true)(
    Record("a" -> redOrBlue), Union(Seq(Record("a" -> redSingleton), Record("a" -> blueSingleton))),
    "{a: Red | Blue}, {a: Red} | {a: Blue}"
  )
  testAssignable(true)(
    Record("a" -> redBlueUnionAliasType), Union(Seq(Record("a" -> redSingleton), Record("a" -> blueSingleton))),
    "{a: RedOrBlueAlias}, {a: Red} | {a: Blue}"
  )
  testAssignable(true)(redOrBlueTaggedType, redOrBlue, "RedOrBlueTagged, Red | Blue")
  testAssignable(true)(
    redOrBlueTaggedType, Union(Seq(redSingleton, blueSingleton, greenSingleton)), "RedOrBlueTagged, Red | Blue | Green"
  )
  testAssignable(false)(
    redOrBlueTaggedType, Union(Seq(redSingleton, greenSingleton)), "RedOrBlueTagged, Red | Green"
  )
  testAssignable(true)(
    Intersection(Seq(redOrBlueTaggedType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "RedOrBlueTagged & C, (Red & C) | (Blue & C)"
  )
  testAssignable(false)(
    Intersection(Seq(redOrBlueTaggedType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(greenSingleton, cParam)))),
    "RedOrBlueTagged & C, (Red & C) | (Green & C)"
  )

  val someCon = WrapperTypeCon(TypeDefKind.Tagged, "Some", Seq(aParam.con), aParam, NoPos)
  def someType(t: Type) = Tagged(someCon, Seq(t), t)
  val noneSingleton = Singleton(SingletonTypeCon("None", NoPos))

  testAssignable(true)(
    someType(redOrBlue), Union(Seq(someType(redSingleton), someType(blueSingleton))),
    "Some<Red | Blue>, Some<Red> | Some<Blue>"
  )
  testAssignable(false)(
    someType(redOrBlue), Union(Seq(someType(redSingleton), someType(greenSingleton))),
    "Some<Red | Blue>, Some<Red> | Some<Green>"
  )
  testAssignable(true)(
    Union(Seq(someType(redOrBlue), noneSingleton)),
    Union(Seq(someType(redSingleton), someType(blueSingleton), noneSingleton)),
    "Some<Red | Blue> | None, Some<Red> | Some<Blue> | None (exhaustive Option match)"
  )
  testAssignable(true)(
    Boxed(justBoxedCon, Seq(redOrBlue), redOrBlue),
    Union(Seq(Boxed(justBoxedCon, Seq(redSingleton), redSingleton), Boxed(justBoxedCon, Seq(blueSingleton), blueSingleton))),
    "JustBoxed<Red | Blue>, JustBoxed<Red> | JustBoxed<Blue>"
  )
  testAssignable(true)(
    redOrBlueTaggedType,
    Union(Seq(Intersection(Seq(redOrBlueTaggedType, redSingleton)), Intersection(Seq(redOrBlueTaggedType, blueSingleton)))),
    "RedOrBlueTagged, (RedOrBlueTagged & Red) | (RedOrBlueTagged & Blue)"
  )

  def allRecords(fieldNames: Seq[String]): Seq[Type] = fieldNames.foldLeft(Seq(Seq.empty[(String, Type)])) {
    case (records, name) => for { fields <- records; t <- Seq(redSingleton, blueSingleton) } yield fields :+ (name -> t)
  }.map(fields => Record(fields: _*))

  testAssignable(true)(
    Record(Seq("a", "b", "c", "d").map(_ -> redOrBlue): _*), Union(allRecords(Seq("a", "b", "c", "d"))),
    "{a..d: Red | Blue}, all 16 combinations"
  )
  testAssignable(false)(
    Record(Seq("a", "b", "c", "d", "e").map(_ -> redOrBlue): _*), Union(allRecords(Seq("a", "b", "c", "d", "e"))),
    "{a..e: Red | Blue}, all 32 combinations (known limit: more than MaxUnionCombinations aren't expanded)"
  )

  // records and tuples with an empty member have no values

  testAssignable(true)(Record("a" -> Intersection(Seq(redSingleton, blueSingleton))), Uni, "{a: Red & Blue}, ()")
  testAssignable(true)(Tuple(Seq(Intersection(Seq(redSingleton, blueSingleton)), Uni)), Uni, "(Red & Blue, ()), ()")
  testAssignable(false)(Record("a" -> Intersection(Seq(aParam, bParam))), Uni, "{a: A & B}, ()")
  testHeadNormalization(
    Intersection(Seq(Record("a" -> Intersection(Seq(redSingleton, blueSingleton))), Record("b" -> Uni))),
    Bottom,
    "record intersection with an empty field that isn't shared reduces to Bottom"
  )

  // intersections of functions, e.g. the type of #(Red => ..., Blue => ...)

  testAssignable(true)(
    Intersection(Seq(Func(redSingleton, Uni), Func(blueSingleton, Uni))), Func(redOrBlue, Uni),
    "(Red => ()) & (Blue => ()), Red | Blue => ()"
  )
  testAssignable(true)(
    Intersection(Seq(Func(redSingleton, Uni), Func(blueSingleton, Uni))), Func(redBlueUnionAliasType, Uni),
    "(Red => ()) & (Blue => ()), RedOrBlueAlias => ()"
  )
  testAssignable(true)(
    Intersection(Seq(Func(redSingleton, Uni), Func(blueSingleton, Uni))), Func(redSingleton, Uni),
    "(Red => ()) & (Blue => ()), Red => ()"
  )
  testAssignable(false)(
    Intersection(Seq(Func(redSingleton, Uni), Func(blueSingleton, Uni))),
    Func(Union(Seq(redSingleton, greenSingleton)), Uni),
    "(Red => ()) & (Blue => ()), Red | Green => ()"
  )
  testAssignable(false)(
    Intersection(Seq(Func(redSingleton, Uni), Func(blueSingleton, Top))), Func(redOrBlue, Uni),
    "(Red => ()) & (Blue => *), Red | Blue => ()"
  )
  testAssignable(true)(
    Intersection(Seq(Func(Tuple(Seq(redSingleton, Uni)), Uni), Func(Tuple(Seq(blueSingleton, Uni)), Uni))),
    Func(Tuple(Seq(redOrBlue, Uni)), Uni),
    "((Red, ()) => ()) & ((Blue, ()) => ()), (Red | Blue, ()) => ()"
  )
}
