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
      val typ = TypeInterpreter.instantiate(con, typeArgs, NoPos).value.toOption
      assert(typ === expected)
    }
  }

  val aParamCon = ParamCon("A", NoPos)
  val aParamExpected = Param(aParamCon)
  val bParamCon = ParamCon("B", NoPos)
  val bParamExpected = Param(bParamCon)
  val cParamCon = ParamCon("C", NoPos)
  val cParamExpected = Param(cParamCon)
  val dParamCon = ParamCon("D", NoPos)
  val dParamExpected = Param(cParamCon)

  testInstantiate(aParamCon, Nil)(Some(aParamExpected))
  testInstantiate(aParamCon, Seq(Uni))(None)

  val unitAliasCon = WrapperTypeCon(TypeDefKind.Alias, "UnitAlias", Nil, Uni, NoPos)
  val unitAliasExpected = Alias(unitAliasCon, Nil, Uni)
  testInstantiate(unitAliasCon, Nil)(Some(unitAliasExpected))
  testInstantiate(unitAliasCon, Seq(Uni))(None)

  val functionAliasCon = WrapperTypeCon(TypeDefKind.Alias, "Function", Seq(aParamCon, bParamCon), Func(aParamExpected, bParamExpected), NoPos)
  val functionAliasExpected = Alias(functionAliasCon, Seq(Uni, cParamExpected), Func(Uni, cParamExpected))
  testInstantiate(functionAliasCon, Seq(Uni, cParamExpected))(Some(functionAliasExpected))
  testInstantiate(functionAliasCon, Seq(Uni))(None)
  testInstantiate(functionAliasCon, Nil)(None)

  val aliasOfAliasCon = WrapperTypeCon(TypeDefKind.Alias, "AliasOfFunction", Seq(cParamCon, dParamCon), Alias(functionAliasCon, Seq(cParamExpected, dParamExpected), Func(cParamExpected, dParamExpected)), NoPos)
  val aliasOfAliasExpected = Alias(aliasOfAliasCon, Seq(Uni, Uni), Alias(functionAliasCon, Seq(Uni, Uni), Func(Uni, Uni)))
  testInstantiate(aliasOfAliasCon, Seq(Uni, Uni))(Some(aliasOfAliasExpected))
  testInstantiate(aliasOfAliasCon, Seq(Uni, Uni, Uni))(None)
  testInstantiate(aliasOfAliasCon, Seq(Uni))(None)
  testInstantiate(aliasOfAliasCon, Nil)(None)

  val nullaryOpaqueCon = OpaqueTypeCon("Foo", Nil, NoPos)
  val nullaryOpaqueExpected = Opaque(nullaryOpaqueCon, Nil)
  testInstantiate(nullaryOpaqueCon, Nil)(Some(nullaryOpaqueExpected))
  testInstantiate(nullaryOpaqueCon, Seq(Uni))(None)

  val unaryOpaqueCon = OpaqueTypeCon("Bar", Seq(aParamCon), NoPos)
  val unaryOpaqueExpected = Opaque(unaryOpaqueCon, Seq(Uni))
  testInstantiate(unaryOpaqueCon, Seq(Uni))(Some(unaryOpaqueExpected))
  testInstantiate(unaryOpaqueCon, Nil)(None)

  val singletonCon = SingletonTypeCon("Singleton", NoPos)
  val singletonExpected = Singleton(singletonCon)
  testInstantiate(singletonCon, Nil)(Some(singletonExpected))
  testInstantiate(singletonCon, Seq(Uni))(None)
}
