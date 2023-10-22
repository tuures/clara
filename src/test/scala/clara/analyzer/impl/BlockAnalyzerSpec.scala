package clara.analyzer.impl

import clara.ast.Ast
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("typeDefs: Block with just one typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Ast.Block(Seq(typeDef))

    val endState = BlockAnalyzer(Env.empty).walkBlockContents(block.bcs)

    val typ = endState.value.value.currentEnv.typeCons.get("Unit").get

    inside(typ) {
      case TypeCons.WrapperTypeCon(typeDefKind, name, typeParams, wrappedType, _, _) =>
        assert((typeDefKind, name, typeParams, wrappedType) === (TypeDefKind.Alias, "Unit", Vector(), Types.Uni))
    }

    val blockTermAn = endState.flatMap(_.finishTerm(block.pos))

    val expectedTerm = Terms.Block(Vector(Terms.TypeDef(typ)), Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.map(_.message) === Vector("Block should end with an expression."))
  }

  // FIXME add test for valueDef and valueDecl


  // FIXME detailed tests for different typeDef kinds should be in TypeDefAnalyzerSpec
  // test("invalid ::opaque Pair<A>: ()") {

  //   val typeDef = TypeDef(TypeDefKind.Opaque, "Pair", Seq(Ast.TypeParam("A")), UnitType())
  //   val block = Ast.Block(Seq(typeDef))

  //   val an = BlockAnalyzer(Env.empty).walkBlock(block)

  //   val expectedErrors = Seq("Cannot define type parameters for type Pair", "Cannot define structure for type Pair")

  //   assert(an.value.left.value.map(_.message) === expectedErrors)
  // }
}
