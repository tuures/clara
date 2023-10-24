package clara.analyzer.impl

import clara.testutil.BaseSpec

class TypeDefAnalyzerSpec extends BaseSpec {

  test("???") {
    ???// TypeDefAnalyzer.typeDefTerm
  }


  // FIXME detailed tests for different typeDef kinds
  // test("invalid ::opaque Pair<A>: ()") {

  //   val typeDef = TypeDef(TypeDefKind.Opaque, "Pair", Seq(Ast.TypeParam("A")), UnitType())
  //   val block = Ast.Block(Seq(typeDef))

  //   val an = BlockAnalyzer(Env.empty).walkBlock(block)

  //   val expectedErrors = Seq("Cannot define type parameters for type Pair", "Cannot define structure for type Pair")

  //   assert(an.value.left.value.map(_.message) === expectedErrors)
  // }
}
