package clara.analyzer.impl

import clara.ast.Ast
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("::alias Unit: ()") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Ast.Block(Seq(typeDef))

    val endState = BlockAnalyzer(Env.empty).walkBlockContents(block.bcs)

    val typ = endState.value.value.currentEnv.typeCons.get("Unit").get

    inside(typ) {
      case TypeCons.WrapperTypeCon(typeDefKind, name, typeParams, wrappedType, _, _) =>
        assert((typeDefKind, name, typeParams, wrappedType) === (TypeDefKind.Alias, "Unit", Vector(), Types.Uni))
    }

    val term = endState.flatMap(_.finishTerm(block.pos)).value.value

    val expectedTerm = Terms.Block(Vector(Terms.TypeDef("Unit"/*FIXME add missing properties*/)), Types.Uni)
    assert(term === expectedTerm)
  }

  test("invalid ::opaque Pair<A>: ()") {

    val typeDef = TypeDef(TypeDefKind.Opaque, "Pair", Seq(Ast.TypeParam("A")), UnitType())
    val block = Ast.Block(Seq(typeDef))

    val an = BlockAnalyzer(Env.empty).walkBlock(block)

    val expectedErrors = Seq("Cannot define type parameters for type Pair", "Cannot define structure for type Pair")

    assert(an.value.left.value.map(_.message) === expectedErrors)
  }
}
