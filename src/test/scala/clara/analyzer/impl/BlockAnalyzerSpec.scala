package clara.analyzer.impl

import clara.ast.Ast
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("typeDef: Block with just one typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Block(Seq(typeDef))

    val endState = BlockAnalyzerImpl(Env.empty).walkBlockContents(block.bcs)

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

  test("typeDef: Allow shadowing from parent scope.") {
    ???
  }

  test("valueDef and valueDecl: Should affect the value env inside the block and match the returned term contents.") {
    ???

    //   ve("Block yields type of the last expression", "String") {
//     Block(Seq(UnitLiteral(), StringLiteral(Seq(LiteralValue.StringPlainPart("foo")))))
//   }
//
  }

  test("valueDef and valueDecl: Allow shadowing from parent scope.") {
    ???
  }

  test("valueExpr: Non-unit returning expression should give warning unless it's the last item in the block") {
    ???
  }

  test("methods: ???") {
    ???
  }
}
