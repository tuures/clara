package clara.analyzer.impl

import clara.ast.NoPos
import clara.ast.Ast.TypeDefKind
import clara.asg.{Types, TypeCons}
import clara.testutil.BaseSpec
import clara.util.Safe.{SafeStringContext, StringsSafeString}

class TypeInterpreterSpec extends BaseSpec {
  import TypeCons._
  import Types._

  def testInstantiate(con: TypeCon, typeArgs: Seq[Type])(expected: Option[Type]) = {
    val conDesc = TypeCons.toSource(con)
    val argsDesc = typeArgs.map(Types.toSource).safeString("<", ", ", ">")
    val expectedDesc = expected.map(Types.toSource).map(t => safe"-> $t").getOrElse("should be a Failure")
    test(safe"instantiate($conDesc, $argsDesc) $expectedDesc") {
      val typ = TypeInterpreter.instantiate(con, typeArgs, NoPos).run.value.toOption
      assert(typ === expected)
    }
  }

  def testWrappedType(t: Wrapper, typeStructures: TypeStructures)(expected: Type) =
    test(safe"wrappedType(${Types.toSource(t)}) -> ${Types.toSource(expected)}") {
      assert(TypeInterpreterImpl(typeStructures).wrappedType(t) === expected)
    }

  val aParamCon = ParamCon("A", NoPos)
  val aParam = Param(aParamCon)
  val bParamCon = ParamCon("B", NoPos)
  val bParam = Param(bParamCon)
  val cParamCon = ParamCon("C", NoPos)
  val cParam = Param(cParamCon)
  val dParamCon = ParamCon("D", NoPos)
  val dParam = Param(dParamCon)

  testInstantiate(aParamCon, Nil)(Some(aParam))
  testInstantiate(aParamCon, Seq(Uni))(None)

  val unitAliasCon = WrapperTypeCon(TypeDefKind.Alias, "UnitAlias", Nil, Nil, NoPos)
  val unitAliasStructure = TypeStructures(Map(unitAliasCon.uniq -> Uni))
  val unitAlias = Alias(unitAliasCon, Nil)
  testInstantiate(unitAliasCon, Nil)(Some(unitAlias))
  testInstantiate(unitAliasCon, Seq(Uni))(None)
  testWrappedType(unitAlias, unitAliasStructure)(Uni)

  val functionAliasCon = WrapperTypeCon(TypeDefKind.Alias, "Function", Seq(aParamCon, bParamCon), Nil, NoPos)
  val functionAliasStructure = TypeStructures(Map(functionAliasCon.uniq -> Func(aParam, bParam)))
  val functionAlias = Alias(functionAliasCon, Seq(Uni, cParam))
  testInstantiate(functionAliasCon, Seq(Uni, cParam))(Some(functionAlias))
  testInstantiate(functionAliasCon, Seq(Uni))(None)
  testInstantiate(functionAliasCon, Nil)(None)
  testWrappedType(functionAlias, functionAliasStructure)(Func(Uni, cParam))

  val aliasOfAliasCon = WrapperTypeCon(TypeDefKind.Alias, "AliasOfFunction", Seq(cParamCon, dParamCon), Nil, NoPos)
  val aliasOfAliasStructure = TypeStructures(Map(aliasOfAliasCon.uniq -> Alias(functionAliasCon, Seq(cParam, dParam))))
  val aliasOfAlias = Alias(aliasOfAliasCon, Seq(Uni, Uni))
  testInstantiate(aliasOfAliasCon, Seq(Uni, Uni))(Some(aliasOfAlias))
  testInstantiate(aliasOfAliasCon, Seq(Uni, Uni, Uni))(None)
  testInstantiate(aliasOfAliasCon, Seq(Uni))(None)
  testInstantiate(aliasOfAliasCon, Nil)(None)
  // an alias of an alias wraps the inner alias, not what the inner one wrapped
  testWrappedType(aliasOfAlias, aliasOfAliasStructure)(Alias(functionAliasCon, Seq(Uni, Uni)))

  val nullaryOpaqueCon = OpaqueTypeCon("Foo", Nil, Nil, NoPos)
  val nullaryOpaque = Opaque(nullaryOpaqueCon, Nil)
  testInstantiate(nullaryOpaqueCon, Nil)(Some(nullaryOpaque))
  testInstantiate(nullaryOpaqueCon, Seq(Uni))(None)

  val unaryOpaqueCon = OpaqueTypeCon("Bar", Seq(aParamCon), Nil, NoPos)
  val unaryOpaque = Opaque(unaryOpaqueCon, Seq(Uni))
  testInstantiate(unaryOpaqueCon, Seq(Uni))(Some(unaryOpaque))
  testInstantiate(unaryOpaqueCon, Nil)(None)

  val singletonCon = SingletonTypeCon("Singleton", Nil, NoPos)
  val singleton = Singleton(singletonCon)
  testInstantiate(singletonCon, Nil)(Some(singleton))
  testInstantiate(singletonCon, Seq(Uni))(None)

  // type relations

  // the relations get the structures of the wrapper types they're given, combined with structures(...)

  def structures(typeStructures: TypeStructures*): TypeStructures = typeStructures.reduce(_.append(_))

  def testHeadNormalization(t1: Type, t2: Type, description: String, typeStructures: TypeStructures = TypeStructures.empty): Unit = {
    test(safe"head normalization: $description") {
      assert(TypeInterpreterImpl(typeStructures).normalizeHead(t1) === t2)
    }
  }

  val aParamCon2 = ParamCon("A", NoPos)
  val bParamCon2 = ParamCon("B", NoPos)
  val aParam2 = Param(aParamCon2)
  val bParam2 = Param(bParamCon2)

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
    Nil,
    NoPos,
  )
  val funcAliasStructure = TypeStructures(Map(funcAliasCon.uniq -> Func(aParam, bParam)))
  val funcUniUniAliasType = Alias(funcAliasCon, Seq(Uni, Uni))

  val justTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "JustTagged", Seq(aParam.con), Nil, NoPos)
  val justTaggedStructure = TypeStructures(Map(justTaggedCon.uniq -> aParam))
  val justUniTaggedType = Tagged(justTaggedCon, Seq(Uni))
  val justTopTaggedType = Tagged(justTaggedCon, Seq(Top))

  val justBoxedCon = WrapperTypeCon(TypeDefKind.Boxed, "JustBoxed", Seq(aParam.con), Nil, NoPos)
  val justBoxedStructure = TypeStructures(Map(justBoxedCon.uniq -> aParam))
  val justUniBoxedType = Boxed(justBoxedCon, Seq(Uni))
  val justTopBoxedType = Boxed(justBoxedCon, Seq(Top))

  val fooOpaqueCon = OpaqueTypeCon("FooOpaque", Seq(aParam.con), Nil, NoPos)
  val fooUniOpaqueType = Opaque(fooOpaqueCon, Seq(Uni))
  val fooTopOpaqueType = Opaque(fooOpaqueCon, Seq(Top))

  val barOpaqueCon = OpaqueTypeCon("BarOpaque", Seq(aParam.con), Nil, NoPos)
  val barUniOpaqueType = Opaque(barOpaqueCon, Seq(Uni))


  val funcUniUniOpaqueType = Opaque(fooOpaqueCon, Seq(Func(Uni, Uni)))
  val funcUniUniAliasOpaqueType = Opaque(fooOpaqueCon, Seq(funcUniUniAliasType))

  val blueSingleton = Singleton(SingletonTypeCon("BlueSingleton", Nil, NoPos))
  val redSingleton = Singleton(SingletonTypeCon("RedSingleton", Nil, NoPos))
  val unusedTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "UnusedTagged", Seq(aParam.con), Nil, NoPos)
  val unusedTaggedStructure = TypeStructures(Map(unusedTaggedCon.uniq -> Uni))
  val handlerTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "Handler", Seq(aParam.con), Nil, NoPos)
  val handlerTaggedStructure = TypeStructures(Map(handlerTaggedCon.uniq -> Func(aParam, Uni)))
  val unusedBoxedCon = WrapperTypeCon(TypeDefKind.Boxed, "UnusedBoxed", Seq(aParam.con), Nil, NoPos)
  val unusedBoxedStructure = TypeStructures(Map(unusedBoxedCon.uniq -> Uni))

  testHeadNormalization(
    Intersection(Seq(blueSingleton, redSingleton)),
    Bottom,
    "disjoint singleton intersections reduce to Bottom"
  )

  val redBlueUnionAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "RedOrBlueAlias",
    Nil,
    Nil,
    NoPos,
  )
  val redBlueUnionAliasStructure = TypeStructures(Map(redBlueUnionAliasCon.uniq -> Union(Seq(redSingleton, blueSingleton))))
  val redBlueUnionAliasType = Alias(redBlueUnionAliasCon, Nil)
  val greenSingleton = Singleton(SingletonTypeCon("GreenSingleton", Nil, NoPos))

  testHeadNormalization(
    Intersection(Seq(redBlueUnionAliasType, greenSingleton)),
    Bottom,
    "intersection with a disjoint alias of a union reduces to Bottom",
    typeStructures = redBlueUnionAliasStructure,
  )
  testHeadNormalization(
    Intersection(Seq(redBlueUnionAliasType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "intersection distributes over an alias of a union",
    typeStructures = redBlueUnionAliasStructure,
  )

  def testDisjoint(expected: Boolean)(t1: Type, t2: Type, description: String, typeStructures: TypeStructures = TypeStructures.empty): Unit = {
    test(safe"${if (expected) "" else "!"}disjoint($description)") {
      val interpreter = TypeInterpreterImpl(typeStructures)
      assert(interpreter.disjoint(t1, t2) === expected)
      assert(interpreter.disjoint(t2, t1) === expected, "disjoint should be symmetric")
    }
  }

  val redGreenTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "RedGreenTagged", Nil, Nil, NoPos)
  val redGreenTaggedStructure = TypeStructures(Map(redGreenTaggedCon.uniq -> Union(Seq(redSingleton, greenSingleton))))
  val redGreenTaggedType = Tagged(redGreenTaggedCon, Nil)

  testDisjoint(true)(Bottom, aParam, "!, A")
  testDisjoint(true)(redSingleton, blueSingleton, "Red, Blue")
  testDisjoint(false)(redSingleton, redSingleton, "Red, Red")
  testDisjoint(true)(redBlueUnionAliasType, greenSingleton, "RedOrBlueAlias, Green", typeStructures = redBlueUnionAliasStructure)
  testDisjoint(false)(redBlueUnionAliasType, redSingleton, "RedOrBlueAlias, Red", typeStructures = redBlueUnionAliasStructure)
  testDisjoint(true)(Intersection(Seq(aParam, redSingleton)), blueSingleton, "A & Red, Blue")
  testDisjoint(false)(redGreenTaggedType, Union(Seq(redSingleton, blueSingleton)), "RedGreenTagged, Red | Blue", typeStructures = redGreenTaggedStructure)
  testDisjoint(true)(redGreenTaggedType, blueSingleton, "RedGreenTagged, Blue", typeStructures = redGreenTaggedStructure)
  testDisjoint(true)(Tuple(Seq(Uni)), Tuple(Seq(Uni, Uni)), "((),), ((), ())")
  testDisjoint(true)(Tuple(Seq(redSingleton, Uni)), Tuple(Seq(blueSingleton, Uni)), "(Red, ()), (Blue, ())")
  testDisjoint(false)(Tuple(Seq(redSingleton, aParam)), Tuple(Seq(redSingleton, Uni)), "(Red, A), (Red, ())")
  testDisjoint(true)(Record("a" -> redSingleton), Record("a" -> blueSingleton), "{a: Red}, {a: Blue}")
  testDisjoint(true)(
    Record("a" -> redSingleton, "b" -> Uni), Record("a" -> blueSingleton), "{a: Red, b: ()}, {a: Blue}"
  )
  testDisjoint(false)(Record("a" -> redSingleton), Record("a" -> redBlueUnionAliasType), "{a: Red}, {a: RedOrBlueAlias}", typeStructures = redBlueUnionAliasStructure)
  testDisjoint(false)(Record("a" -> redSingleton), Record("b" -> blueSingleton), "{a: Red}, {b: Blue}")
  testDisjoint(true)(justUniBoxedType, Boxed(unusedBoxedCon, Seq(Uni)), "JustBoxed<()>, UnusedBoxed<()>", typeStructures = structures(justBoxedStructure, unusedBoxedStructure))
  testDisjoint(true)(
    Boxed(justBoxedCon, Seq(redSingleton)), Boxed(justBoxedCon, Seq(blueSingleton)),
    "JustBoxed<Red>, JustBoxed<Blue>",
    typeStructures = justBoxedStructure,
  )
  testDisjoint(true)(justUniBoxedType, redSingleton, "JustBoxed<()>, Red", typeStructures = justBoxedStructure)
  testDisjoint(true)(fooUniOpaqueType, barUniOpaqueType, "FooOpaque<()>, BarOpaque<()>")
  testDisjoint(false)(fooUniOpaqueType, Opaque(fooOpaqueCon, Seq(Top)), "FooOpaque<()>, FooOpaque<*> (same opaque type)")
  testDisjoint(true)(fooUniOpaqueType, redSingleton, "FooOpaque<()>, Red")
  testDisjoint(true)(fooUniOpaqueType, justUniBoxedType, "FooOpaque<()>, JustBoxed<()>", typeStructures = justBoxedStructure)
  // conservative: overlap can't be ruled out
  testDisjoint(false)(aParam, redSingleton, "A, Red")
  testDisjoint(false)(aParam, bParam, "A, B")
  testDisjoint(false)(fooUniOpaqueType, Record("a" -> Uni), "FooOpaque<()>, {a: ()}")

  def testAssignable(expected: Boolean)(t1: Type, t2: Type, description: String = "", typeStructures: TypeStructures = TypeStructures.empty): Unit = {
    val desc = if (description.length > 0) description else safe"${t1.toString()}, ${t2.toString()}"
    test(safe"${if(expected) "" else "!"}isAssignable($desc)") {
      assert(TypeInterpreterImpl(typeStructures).isAssignable(t1, t2) === expected)
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
  testAssignable(true)(
    Intersection(Seq(redBlueUnionAliasType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "RedOrBlueAlias & C, (Red & C) | (Blue & C)",
    typeStructures = redBlueUnionAliasStructure,
  )
  testAssignable(true)(
    Intersection(Seq(redSingleton, blueSingleton)), aParam,
    "Red & Blue, A (empty intersection is assignable to anything)"
  )
  testAssignable(false)(
    Uni, Intersection(Seq(redSingleton, blueSingleton)),
    "(), Red & Blue"
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

  testAssignable(true)(
    funcUniUniAliasType, funcUniUniAliasType,
    "FunctionAlias<Uni, Uni>, FunctionAlias<Uni, Uni>",
    typeStructures = funcAliasStructure
  )
  testAssignable(true)(
    Func(Uni, Uni), funcUniUniAliasType,
    "Func(Uni, Uni), FunctionAlias<Uni, Uni>",
    typeStructures = funcAliasStructure
  )
  testAssignable(true)(
    funcUniUniAliasType, Func(Uni, Uni),
    "FunctionAlias<Uni, Uni>, Func(Uni, Uni>",
    typeStructures = funcAliasStructure
  )
  testAssignable(true)(
    funcUniUniAliasType, Func(Uni, Top),
    "FunctionAlias<Uni, Uni>, Func(Uni, Top)",
    typeStructures = funcAliasStructure
  )
  testAssignable(false)(
    funcUniUniAliasType, Func(Top, Uni),
    "FunctionAlias<Uni, Uni>, Func(Top, Uni)",
    typeStructures = funcAliasStructure
  )
  testAssignable(true)(
    Func(Top, Uni), funcUniUniAliasType,
    "Func(Top, Uni), FunctionAlias<Uni, Uni>",
    typeStructures = funcAliasStructure
  )
  testAssignable(false)(
    Func(Uni, Top), funcUniUniAliasType,
    "Func(Uni, Top), FunctionAlias<Uni, Uni>",
    typeStructures = funcAliasStructure
  )

  testAssignable(true)(
    justUniTaggedType, justUniTaggedType,
    "JustTagged<Uni>, JustTagged<Uni>",
    typeStructures = justTaggedStructure
  )
  testAssignable(true)(
    justUniTaggedType, justTopTaggedType,
    "JustTagged<Uni>, JustTagged<Top>",
    typeStructures = justTaggedStructure
  )
  testAssignable(false)(
    justTopTaggedType, justUniTaggedType,
    "JustTagged<Top>, JustTagged<Uni>",
    typeStructures = justTaggedStructure
  )
  testAssignable(false)(
    Tagged(unusedTaggedCon, Seq(aParam)), Tagged(unusedTaggedCon, Seq(bParam)),
    "UnusedTagged<A>, UnusedTagged<B>",
    typeStructures = unusedTaggedStructure,
  )
  testAssignable(true)(
    Tagged(handlerTaggedCon, Seq(Top)), Tagged(handlerTaggedCon, Seq(Uni)),
    "Handler<*>, Handler<()> (contravariant)",
    typeStructures = handlerTaggedStructure,
  )
  testAssignable(false)(
    Tagged(handlerTaggedCon, Seq(Uni)), Tagged(handlerTaggedCon, Seq(Top)),
    "Handler<()>, Handler<*> (contravariant)",
    typeStructures = handlerTaggedStructure,
  )
  testAssignable(true)(justUniTaggedType, Uni, "JustTagged<Uni>, Uni", typeStructures = justTaggedStructure)
  testAssignable(false)(Uni, justUniTaggedType, "Uni, JustTagged<Uni>", typeStructures = justTaggedStructure)

  testAssignable(true)(justUniBoxedType, justUniBoxedType, "JustBoxed<Uni>, JustBoxed<Uni>", typeStructures = justBoxedStructure)
  testAssignable(true)(justUniBoxedType, justTopBoxedType, "JustBoxed<Uni>, JustBoxed<Top>", typeStructures = justBoxedStructure)
  testAssignable(false)(justTopBoxedType, justUniBoxedType, "JustBoxed<Top>, JustBoxed<Uni>", typeStructures = justBoxedStructure)
  testAssignable(false)(
    Boxed(unusedBoxedCon, Seq(aParam)), Boxed(unusedBoxedCon, Seq(bParam)),
    "UnusedBoxed<A>, UnusedBoxed<B>",
    typeStructures = unusedBoxedStructure,
  )
  testAssignable(false)(justUniBoxedType, Uni, "JustBoxed<Uni>, Uni", typeStructures = justBoxedStructure)
  testAssignable(false)(Uni, justUniBoxedType, "Uni, JustBoxed<Uni>", typeStructures = justBoxedStructure)

  testAssignable(true)(fooUniOpaqueType, fooUniOpaqueType, "FooOpaque<Uni>, FooOpaque<Uni>")
  testAssignable(false)(fooUniOpaqueType, fooTopOpaqueType, "FooOpaque<Uni>, FooOpaque<Top>")
  testAssignable(false)(fooTopOpaqueType, fooUniOpaqueType, "FooOpaque<Top>, FooOpaque<Uni>")
  testAssignable(false)(fooUniOpaqueType, Uni, "FooOpaque<Uni>, Uni")
  testAssignable(false)(Uni, fooUniOpaqueType, "Uni, FooOpaque<Uni>")

  testAssignable(false)(fooUniOpaqueType, barUniOpaqueType, "FooOpaque<Uni>, BarOpaque<Uni>")
  testAssignable(false)(barUniOpaqueType, fooUniOpaqueType, "BarOpaque<Uni>, FooOpaque<Uni>")

  testAssignable(true)(
    funcUniUniOpaqueType, funcUniUniAliasOpaqueType,
    "FooOpaque<Uni => Uni>, FooOpaque<FunctionAlias<Uni, Uni>>",
    typeStructures = funcAliasStructure,
  )
  testAssignable(true)(
    funcUniUniAliasOpaqueType, funcUniUniOpaqueType,
    "FooOpaque<FunctionAlias<Uni, Uni>>, FooOpaque<Uni => Uni>",
    typeStructures = funcAliasStructure,
  )

  testAssignable(true)(blueSingleton, blueSingleton, "BlueSingleton, BlueSingleton")
  testAssignable(false)(blueSingleton, redSingleton, "BlueSingleton, RedSingleton")
  testAssignable(false)(redSingleton, blueSingleton, "RedSingleton, BlueSingleton")

  testAssignable(true)(
    funcUniUniAliasType, Union(Seq(Func(Uni, Uni), Uni)),
    "FunctionAlias<Uni, Uni>, (Uni => Uni) | Uni",
    typeStructures = funcAliasStructure,
  )
  testAssignable(true)(
    Intersection(Seq(Func(Uni, Uni), Uni)), funcUniUniAliasType,
    "(Uni => Uni) | Uni, FunctionAlias<Uni, Uni>",
    typeStructures = funcAliasStructure,
  )
  testAssignable(true)(
    redBlueUnionAliasType, Union(Seq(redSingleton, blueSingleton)),
    "RedOrBlueAlias, RedSingleton | BlueSingleton",
    typeStructures = redBlueUnionAliasStructure,
  )
  testAssignable(true)(
    Intersection(Seq(redSingleton, blueSingleton)), redBlueUnionAliasType,
    "RedSingleton | BlueSingleton, RedOrBlueAlias",
    typeStructures = redBlueUnionAliasStructure,
  )

  // unions nested in tuples, records and tagged types are expanded when the target is a union

  val redOrBlue = Union(Seq(redSingleton, blueSingleton))
  val redOrBlueTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "RedOrBlueTagged", Nil, Nil, NoPos)
  val redOrBlueTaggedStructure = TypeStructures(Map(redOrBlueTaggedCon.uniq -> redOrBlue))
  val redOrBlueTaggedType = Tagged(redOrBlueTaggedCon, Nil)

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
    "{a: RedOrBlueAlias}, {a: Red} | {a: Blue}",
    typeStructures = redBlueUnionAliasStructure,
  )
  testAssignable(true)(redOrBlueTaggedType, redOrBlue, "RedOrBlueTagged, Red | Blue", typeStructures = redOrBlueTaggedStructure)
  testAssignable(true)(
    redOrBlueTaggedType, Union(Seq(redSingleton, blueSingleton, greenSingleton)), "RedOrBlueTagged, Red | Blue | Green",
    typeStructures = redOrBlueTaggedStructure,
  )
  testAssignable(false)(
    redOrBlueTaggedType, Union(Seq(redSingleton, greenSingleton)), "RedOrBlueTagged, Red | Green",
    typeStructures = redOrBlueTaggedStructure,
  )
  testAssignable(true)(
    Intersection(Seq(redOrBlueTaggedType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(blueSingleton, cParam)))),
    "RedOrBlueTagged & C, (Red & C) | (Blue & C)",
    typeStructures = redOrBlueTaggedStructure,
  )
  testAssignable(false)(
    Intersection(Seq(redOrBlueTaggedType, cParam)),
    Union(Seq(Intersection(Seq(redSingleton, cParam)), Intersection(Seq(greenSingleton, cParam)))),
    "RedOrBlueTagged & C, (Red & C) | (Green & C)",
    typeStructures = redOrBlueTaggedStructure,
  )

  val someCon = WrapperTypeCon(TypeDefKind.Tagged, "Some", Seq(aParam.con), Nil, NoPos)
  val someStructure = TypeStructures(Map(someCon.uniq -> aParam))
  def someType(t: Type) = Tagged(someCon, Seq(t))
  val noneSingleton = Singleton(SingletonTypeCon("None", Nil, NoPos))

  testAssignable(true)(
    someType(redOrBlue), Union(Seq(someType(redSingleton), someType(blueSingleton))),
    "Some<Red | Blue>, Some<Red> | Some<Blue>",
    typeStructures = someStructure,
  )
  testAssignable(false)(
    someType(redOrBlue), Union(Seq(someType(redSingleton), someType(greenSingleton))),
    "Some<Red | Blue>, Some<Red> | Some<Green>",
    typeStructures = someStructure,
  )
  testAssignable(true)(
    Union(Seq(someType(redOrBlue), noneSingleton)),
    Union(Seq(someType(redSingleton), someType(blueSingleton), noneSingleton)),
    "Some<Red | Blue> | None, Some<Red> | Some<Blue> | None (exhaustive Option match)",
    typeStructures = someStructure,
  )
  testAssignable(true)(
    Boxed(justBoxedCon, Seq(redOrBlue)),
    Union(Seq(Boxed(justBoxedCon, Seq(redSingleton)), Boxed(justBoxedCon, Seq(blueSingleton)))),
    "JustBoxed<Red | Blue>, JustBoxed<Red> | JustBoxed<Blue>",
    typeStructures = justBoxedStructure,
  )
  testAssignable(true)(
    redOrBlueTaggedType,
    Union(Seq(Intersection(Seq(redOrBlueTaggedType, redSingleton)), Intersection(Seq(redOrBlueTaggedType, blueSingleton)))),
    "RedOrBlueTagged, (RedOrBlueTagged & Red) | (RedOrBlueTagged & Blue)",
    typeStructures = redOrBlueTaggedStructure,
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
    "(Red => ()) & (Blue => ()), RedOrBlueAlias => ()",
    typeStructures = redBlueUnionAliasStructure,
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
