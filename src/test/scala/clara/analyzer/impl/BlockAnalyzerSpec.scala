package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  test("empty Block (program block) should produce unit type with no warning about missing expression") {
    val block = Block(Seq.empty)

    val blockTermAn = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    val expectedTerm = Terms.Block(Seq.empty, Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.warnings.isEmpty)
  }

  test("typeDef: Block containing only a typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Block(Seq(typeDef))

    val endState = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    val typ = endState.value.value.currentEnv.getTypeCon("Unit").get

    inside(typ) {
      case TypeCons.WrapperTypeCon(typeDefKind, name, typeParams, wrappedType, _, _) =>
        assert((typeDefKind, name, typeParams, wrappedType) === (TypeDefKind.Alias, "Unit", Nil, Types.Uni))
    }

    val blockTermAn = BlockAnalyzerImpl.finishBlock(endState, block.pos, isProgramBlock = false)

    val expectedTerm = Terms.Block(Seq(Terms.TypeDef(typ)), Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.warnings.map(_.message) === Seq(
      "Unused type `Unit`",
      "Block should end with an expression. Implicitly returning unit.",
    ))
  }

  test("valueDef and valueDecl should affect the value env and returned term contents") {
    val block = Block(Seq(
      ValueDecl("declared", UnitType()),
      ValueDef(CapturePattern("defined"), UnitLiteral()),
      Tuple(Seq(NamedValue("declared"), NamedValue("defined"))),
    ))

    val endState = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(endState.value.value.currentEnv.getValue("declared").map(_.typ) === Some(Types.Uni))
    assert(endState.value.value.currentEnv.getValue("defined").map(_.typ) === Some(Types.Uni))

    val blockTermAn = BlockAnalyzerImpl.finishBlock(endState, block.pos, isProgramBlock = false)

    val expectedTerm = Terms.Block(Seq(
      Terms.ValueDecl("declared"),
      Terms.ValueDef(Terms.CapturePattern("defined", Types.Uni), Terms.UnitLiteral()),
      Terms.Tuple(
        Seq(Terms.NamedValue("declared", Types.Uni), Terms.NamedValue("defined", Types.Uni)),
        Types.Tuple(Seq(Types.Uni, Types.Uni)),
      ),
    ), Types.Tuple(Seq(Types.Uni, Types.Uni)))
    assert(blockTermAn.value.value === expectedTerm)
    assert(blockTermAn.log.warnings.isEmpty)
  }

  test("valueDecl should fail when shadowing an existing local value") {
    val block = Block(Seq(
      ValueDecl("value", UnitType()),
      ValueDecl("value", UnitType()),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing value with same name `value`"))
  }

  test("typeDef should fail when shadowing an existing local type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  test("valueDecl should allow shadowing a value from the parent scope") {
    val parentEnv = Env.empty.addOrShadowValue("value", Types.Uni, NoPos).value.value
    val parentValue = parentEnv.getValue("value").get
    val block = Block(Seq(ValueDecl("value", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs)

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.currentEnv.local.values.get("value").get.uniq !== parentValue.uniq)
  }

  test("typeDef should allow shadowing a type from the parent scope") {
    val parentEnv = TypeDefAnalyzer.typeDefTerm(
      Env.empty,
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ).value.value._1
    val parentType = parentEnv.getTypeCon("Type").get
    val block = Block(Seq(TypeDef(TypeDefKind.Alias, "Type", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs)

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.currentEnv.local.typeCons.get("Type").get.uniq !== parentType.uniq)
  }

  test("lambda parameter should allow shadowing an outer value") {
    val block = Block(Seq(
      ValueDef(CapturePattern("x"), UnitLiteral()),
      ValueDef(CapturePattern("f"), Lambda(
        PatternAs(CapturePattern("x"), UnitType()),
        NamedValue("x"),
      )),
      NamedValue("f"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Unused value `x`"))
  }

  test("lambda type parameter should allow shadowing an outer type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "T", UnitType()),
      ValueDef(CapturePattern("f"), Lambda(
        Seq(Ast.TypeParam("T")),
        PatternAs(CapturePattern("x"), NamedType("T")),
        UnitLiteral(),
      )),
      NamedValue("f"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Unused type `T`"))
  }

  test("block warns about unused local values") {
    val block = Block(Seq(
      ValueDecl("unused", UnitType()),
      ValueDecl("used", UnitType()),
      NamedValue("used"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Seq("Unused value `unused`"))
  }

  test("block warns about unused local types") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Unused", UnitType()),
      TypeDef(TypeDefKind.Alias, "Used", UnitType()),
      ValueDecl("typed", NamedType("Used")),
      NamedValue("typed"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Seq("Unused type `Unused`"))
  }

  test("use in a nested block counts as use of the parent block local") {
    val outer = Block(Seq(
      ValueDecl("x", UnitType()),
      Block(Seq(NamedValue("x"))),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, outer)

    assert(result.log.warnings.isEmpty)
  }

  test("program block: Non-unit last expression should warn about discarded value") {
    def lambdaAst = Lambda(Nil, UnitPattern(), UnitLiteral())
    def lambdaTerm = Terms.Lambda(Terms.UnitPattern(), Terms.UnitLiteral(), Types.Func(Nil, Types.Uni, Types.Uni))

    val block = Block(Seq(lambdaAst, lambdaAst))

    val blockTermAn = BlockAnalyzer.programBlockTerm(Env.empty, block)

    val expectedTerm = Terms.Block(Seq(lambdaTerm, lambdaTerm), Types.Func(Nil, Types.Uni, Types.Uni))
    assert(blockTermAn.value.value === expectedTerm)

    val expectedErrors = Seq("Non-unit value discarded in block", "Non-unit value discarded in program")
    assert(blockTermAn.log.warnings.map(_.message) === expectedErrors)
  }

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

    val blockTermAn = BlockAnalyzer.regularBlockTerm(Env.empty, block)

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

    assert(blockTermAn.log.warnings.map(_.message) === Seq.fill(3)("Non-unit value discarded in block"))
  }

  // TODO
  // test("methods: ???") {
  //   ???
  // }

  // test("methods: ???") {
  //   ???
  // }
}
