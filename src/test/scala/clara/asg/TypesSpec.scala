package clara.asg

import clara.ast.{Ast, NoPos}
import clara.util.Safe.SafeStringContext

import clara.testutil.BaseSpec

class TypesSpec extends BaseSpec {
  import TypeCons._
  import Types._

  val aParam = Param(TypeCons.ParamCon("A", NoPos))
  val aParam2 = Param(TypeCons.ParamCon("A", NoPos))
  val bParam = Param(TypeCons.ParamCon("B", NoPos))

  val funcUniUniAliasType = Alias(
    TypeCons.WrapperTypeCon(
      Ast.TypeDefKind.Alias,
      "Function",
      Seq(aParam.con, bParam.con),
      Func(aParam, bParam),
      NoPos,
    ),
    Seq(Uni, Uni),
    Func(Uni, Uni),
  )

  val fooRecordTaggedType = Tagged(
    TypeCons.WrapperTypeCon(
      Ast.TypeDefKind.Tagged,
      "Foo",
      Seq(aParam.con),
      Record("foo" -> aParam),
      NoPos,
    ),
    Seq(Uni),
    Record("foo" -> Uni),
  )

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

  testAssignable(true)(funcUniUniAliasType, funcUniUniAliasType, "funcUniUniAliasType, funcUniUniAliasType")
  testAssignable(true)(Func(Uni, Uni), funcUniUniAliasType, "Func(Uni, Uni), funcUniUniAliasType")
  testAssignable(true)(funcUniUniAliasType, Func(Uni, Uni), "funcUniUniAliasType, Func(Uni, Uni)")
  testAssignable(true)(funcUniUniAliasType, Func(Uni, Top), "funcUniUniAliasType, Func(Uni, Top)")
  testAssignable(false)(funcUniUniAliasType, Func(Top, Uni), "funcUniUniAliasType, Func(Top, Uni)")
  testAssignable(true)(Func(Top, Uni), funcUniUniAliasType, "Func(Top, Uni), funcUniUniAliasType")
  testAssignable(false)(Func(Uni, Top), funcUniUniAliasType, "Func(Uni, Top), funcUniUniAliasType")

  testAssignable(true)(fooRecordTaggedType, fooRecordTaggedType, "fooRecordTaggedType, fooRecordTaggedType")
  // TODO testAssignable Tagged, Boxed, Opaque, Singleton

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
  testToSource(TypeCons.ParamCon("A", NoPos))("A")
  testToSource(Param(TypeCons.ParamCon("A", NoPos)))("A")
  testToSource(TypeCons.WrapperTypeCon(Ast.TypeDefKind.Alias, "Unit", Nil, Uni, NoPos))("Unit")
  testToSource(Alias(TypeCons.WrapperTypeCon(Ast.TypeDefKind.Alias, "Unit", Nil, Uni, NoPos), Nil, Uni))("Unit")
  testToSource(funcUniUniAliasType.con)("Function<A, B>")
  testToSource(funcUniUniAliasType)("Function<(), ()>")
}
