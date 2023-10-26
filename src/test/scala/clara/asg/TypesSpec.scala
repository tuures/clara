package clara.asg

import clara.ast.NoPos
import clara.ast.Ast.TypeDefKind
import clara.util.Safe.SafeStringContext

import clara.testutil.BaseSpec

class TypesSpec extends BaseSpec {
  import TypeCons._
  import Types._

  val aParam = Param(ParamCon("A", NoPos))
  val aParam2 = Param(ParamCon("A", NoPos))
  val bParam = Param(ParamCon("B", NoPos))

  val funcAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "FunctionAlias",
    Seq(aParam.con, bParam.con),
    Func(aParam, bParam),
    NoPos,
  )
  val funcUniUniAliasType = Alias(funcAliasCon, Seq(Uni, Uni), Func(Uni, Uni))

  var justTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "JustTagged", Seq(aParam.con), aParam, NoPos)
  val justUniTaggedType = Tagged(justTaggedCon, Seq(Uni), Uni)
  val justTopTaggedType = Tagged(justTaggedCon, Seq(Top), Top)

  var justBoxedCon = WrapperTypeCon(TypeDefKind.Tagged, "JustBoxed", Seq(aParam.con), aParam, NoPos)
  val justUniBoxedType = Tagged(justBoxedCon, Seq(Uni), Uni)
  val justTopBoxedType = Tagged(justBoxedCon, Seq(Top), Top)

  val fooOpaqueCon = OpaqueTypeCon("FooOpaque", Seq(aParam.con), NoPos)
  val fooUniOpaqueType = Opaque(fooOpaqueCon, Seq(Uni))
  val fooTopOpaqueType = Opaque(fooOpaqueCon, Seq(Top))

  val barOpaqueCon = OpaqueTypeCon("BarOpaque", Seq(aParam.con), NoPos)
  val barUniOpaqueType = Opaque(barOpaqueCon, Seq(Uni))

  val funcUniUniOpaqueType = Opaque(fooOpaqueCon, Seq(Func(Uni, Uni)))
  val funcUniUniAliasOpaqueType = Opaque(fooOpaqueCon, Seq(funcUniUniAliasType))

  val blueSingleton = Singleton(SingletonTypeCon("BlueSingleton", NoPos))
  val redSingleton = Singleton(SingletonTypeCon("RedSingleton", NoPos))

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

  testAssignable(true)(Func(Uni, Uni), Func(Uni, Uni))
  testAssignable(true)(Func(Uni, Uni), Func(Uni, Top))
  testAssignable(false)(Func(Uni, Uni), Func(Top, Uni))
  testAssignable(true)(Func(Top, Uni), Func(Uni, Uni))
  testAssignable(false)(Func(Uni, Top), Func(Uni, Uni))

  testAssignable(true)(Record(), Record())
  testAssignable(true)(Record("foo" -> Uni), Record("foo" -> Uni))
  testAssignable(true)(Record("foo" -> Uni), Record("foo" -> Top))
  testAssignable(false)(Record("foo" -> Top), Record("foo" -> Uni))
  testAssignable(false)(Record("foo" -> Uni), Record("bar" -> Uni))
  testAssignable(true)(Record("foo" -> Uni, "zot" -> Uni), Record("foo" -> Uni))
  testAssignable(false)(Record("foo" -> Uni), Record("foo" -> Uni, "zot" -> Uni))

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
  testAssignable(false)(justUniTaggedType, Uni, "JustTagged<Uni>, Uni")
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
