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

  testSimplification(Union(Nil), Bottom, "empty union = Bottom")
  testSimplification(Intersection(Nil), Top, "empty intersection = Top")
  testSimplification(Union(Seq(Uni)), Uni, "single member union is the member")
  testSimplification(Intersection(Seq(Uni)), Uni, "single member intersection is the member")

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

  val aParam = Param(aParamCon)
  val aParam2 = Param(aParamCon2)
  val bParam = Param(bParamCon)
  val bParam2 = Param(bParamCon2)

  val blueSingleton = Singleton(SingletonTypeCon("BlueSingleton", Nil, NoPos))
  val redSingleton = Singleton(SingletonTypeCon("RedSingleton", Nil, NoPos))

  val funcAliasCon = WrapperTypeCon(
    TypeDefKind.Alias,
    "FunctionAlias",
    Seq(aParam.con, bParam.con),
    Nil,
    NoPos,
  )
  val funcUniUniAliasType = Alias(funcAliasCon, Seq(Uni, Uni))

  val justTaggedCon = WrapperTypeCon(TypeDefKind.Tagged, "JustTagged", Seq(aParam.con), Nil, NoPos)

  val justBoxedCon = WrapperTypeCon(TypeDefKind.Boxed, "JustBoxed", Seq(aParam.con), Nil, NoPos)

  val fooOpaqueCon = OpaqueTypeCon("FooOpaque", Seq(aParam.con), Nil, NoPos)
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
    val substitutions = Map[TypeConId, Type](
      aParamCon.uniq -> Uni,
      bParamCon.uniq -> Top,
    )
    val typ = Func(
      Seq(aParamCon2, bParamCon2),
      Tuple(Seq(
        aParam,
        Record("value" -> bParam),
        Union(Seq(aParam, bParam)),
        Alias(funcAliasCon, Seq(aParam, bParam)),
        Tagged(justTaggedCon, Seq(aParam)),
        Boxed(justBoxedCon, Seq(bParam)),
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
        Alias(funcAliasCon, Seq(Uni, Top)),
        Tagged(justTaggedCon, Seq(Uni)),
        Boxed(justBoxedCon, Seq(Top)),
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
  testToSource(WrapperTypeCon(TypeDefKind.Alias, "Unit", Nil, Nil, NoPos))("Unit")
  testToSource(Alias(WrapperTypeCon(TypeDefKind.Alias, "Unit", Nil, Nil, NoPos), Nil))("Unit")
  testToSource(funcUniUniAliasType.con)("FunctionAlias<A, B>")
  testToSource(funcUniUniAliasType)("FunctionAlias<(), ()>")
}
