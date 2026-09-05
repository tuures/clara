package clara.asg

import clara.ast.NoPos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe.SafeStringContext

import clara.testutil.BaseSpec

class TypesSpec extends BaseSpec {
  import TypeCons._
  import Types._

  def testNormalization(t1: Type, t2: Type, description: String): Unit = {
    test(safe"normalization: $description") {
      assert(t1 === t2)
    }
  }

  testNormalization(Union(Nil), Bottom, "empty union = Bottom")
  testNormalization(Intersection(Nil), Top, "empty intersection = Top")

  testNormalization(
    Union(Seq(Uni, Uni)), Union(Seq(Uni)),
    "indentical representations removed: () | () = ()"
  )
  testNormalization(
    Intersection(Seq(Uni, Uni)), Intersection(Seq(Uni)),
    "indentical representations removed: () & () = ()"
  )

  val aParamCon = ParamCon("A", NoPos)
  val aParamCon2 = ParamCon("A", NoPos)
  val bParamCon = ParamCon("B", NoPos)
  val bParamCon2 = ParamCon("B", NoPos)

  val aParam = Param(aParamCon)
  val aParam2 = Param(aParamCon2)
  val bParam = Param(bParamCon)
  val bParam2 = Param(bParamCon2)

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

  val redBlueUnionAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "RedOrBlueAlias",
    Nil,
    Union(Seq(redSingleton, blueSingleton)),
    NoPos,
  )
  val redBlueUnionAliasType = Alias(redBlueUnionAliasCon, Nil, Union(Seq(redSingleton, blueSingleton)))

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
  testAssignable(true)(justUniTaggedType, Uni, "JustTagged<Uni>, Uni")
  testAssignable(false)(Uni, justUniTaggedType, "Uni, JustTagged<Uni>")

  testAssignable(true)(justUniBoxedType, justUniBoxedType, "JustBoxed<Uni>, JustBoxed<Uni>")
  testAssignable(true)(justUniBoxedType, justTopBoxedType, "JustBoxed<Uni>, JustBoxed<Top>")
  testAssignable(false)(justTopBoxedType, justUniBoxedType, "JustBoxed<Top>, JustBoxed<Uni>")
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
}
