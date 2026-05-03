package clara.analyzer.impl

import clara.ast.Ast
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("empty Block (program block) should produce unit type with no warning about missing expression") {
    val block = Block(Seq.empty)

    val blockTermAn = BlockAnalyzer.blockTerm(Env.empty, block)

    val expectedTerm = Terms.Block(Seq.empty, Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.isEmpty)
  }

  test("typeDef: Block containing only a typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Block(Seq(typeDef))

    val endState = BlockAnalyzerImpl(Env.empty).walkBlockContents(block.bcs)

    val typ = endState.value.value.currentEnv.typeCons.get("Unit").get

    inside(typ) {
      case TypeCons.WrapperTypeCon(typeDefKind, name, typeParams, wrappedType, _, _) =>
        assert((typeDefKind, name, typeParams, wrappedType) === (TypeDefKind.Alias, "Unit", Nil, Types.Uni))
    }

    val blockTermAn = endState.flatMap(_.finishTerm(block.pos))

    val expectedTerm = Terms.Block(Seq(Terms.TypeDef(typ)), Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.map(_.message) === Seq("Block should end with an expression. Implicitly returning unit."))
  }


  // TODO
  // test("typeDef: Allow shadowing from parent scope.") {
  //   ???
  // }

  // TODO
  // test("valueDef and valueDecl: Should affect the value env inside the block and match the returned term contents.") {
  //     ???

  //     //   ve("Block yields type of the last expression", "String") {
  // //     Block(Seq(UnitLiteral(), StringLiteral(Seq(LiteralValue.StringPlainPart("foo")))))
  // //   }
  // //
  //   }

  // TODO
  // test("valueDef and valueDecl: Allow shadowing from parent scope.") {
  //   ???
  // }

  // TODO
  // test("valueDef and valueDecl: Warn about unused variables.") {
  //   ???
  // }

  test("valueExpr: Non-unit returning expression should give warning unless it's the last item in the block") {
    def lambdaAst = Lambda(Nil, UnitPattern(), UnitLiteral())
    def lambdaTerm = Terms.Lambda(Terms.UnitPattern(), Terms.UnitLiteral(), Types.Func(Nil, Types.Uni, Types.Uni))

    val block = Block(Seq(
      lambdaAst,
      lambdaAst,
      UnitLiteral(),
      UnitLiteral(),
      lambdaAst,
      lambdaAst,
    ))

    val blockTermAn = BlockAnalyzer.blockTerm(Env.empty, block)

    val expectedBlockTermBody = Seq(
      lambdaTerm,
      lambdaTerm,
      Terms.UnitLiteral(),
      Terms.UnitLiteral(),
      lambdaTerm,
      lambdaTerm,
    )
    val expectedTerm = Terms.Block(expectedBlockTermBody, Types.Func(Nil, Types.Uni, Types.Uni))
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.map(_.message) === Seq.fill(3)("Non-unit value discarded in block"))
  }

  // TODO
  // test("methods: ???") {
  //   ???
  // }

  // test("methods: ???") {
  //   ???
  // }
}
