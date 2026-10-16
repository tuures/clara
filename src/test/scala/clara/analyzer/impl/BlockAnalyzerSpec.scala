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

    val typ = endState.value.value.currentEnv.getTypeCon("Unit").get.con

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

  test("typeDef: a typeDef after the last expression should still give a warning of missing expression") {
    // type defs are analyzed before the values, but their position still determines the block's value
    val block = Block(Seq(
      UnitLiteral(),
      TypeDef(TypeDefKind.Singleton, "Last"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Seq(
      "Unused type `Last`",
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

  // forward references: type defs are analyzed before the values of the block, in dependency order

  test("a value can refer to a type defined below it") {
    val block = Block(Seq(
      ValueDecl("x", NamedType("Below")),
      TypeDef(TypeDefKind.Alias, "Below", UnitType()),
      NamedValue("x"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    inside(result.value.value) { case Terms.Block(Seq(Terms.TypeDef(belowCon: TypeCons.WrapperTypeCon), _*), typ) =>
      assert(typ === Types.Alias(belowCon, Nil, Types.Uni))
    }
    assert(result.log.warnings.isEmpty)
  }

  test("a type def can refer to a type defined below it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Pair", TupleType(Seq(NamedType("Below"), NamedType("Below")))),
      TypeDef(TypeDefKind.Singleton, "Below"),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    inside(result.value.value.currentContents) {
      case Seq(Terms.TypeDef(belowCon: TypeCons.SingletonTypeCon), Terms.TypeDef(pairCon: TypeCons.WrapperTypeCon)) =>
        val below = Types.Singleton(belowCon)
        assert(pairCon.wrappedType === Types.Tuple(Seq(below, below)))
    }
  }

  test("type defs come first in the block term, in dependency order") {
    val block = Block(Seq(
      ValueDecl("x", NamedType("Dependent")),
      TypeDef(TypeDefKind.Alias, "Dependent", NamedType("Dependency")),
      NamedValue("x"),
      TypeDef(TypeDefKind.Singleton, "Dependency"),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(result.value.value.currentContents.map(_.getClass.getSimpleName) === Seq("TypeDef", "TypeDef", "ValueDecl", "NamedValue"))
    assert(result.value.value.currentContents.collect { case Terms.TypeDef(con) => con.name } === Seq("Dependency", "Dependent"))
  }

  test("recursive type defs are an error") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "A", TupleType(Seq(NamedType("B"), UnitType()))),
      TypeDef(TypeDefKind.Alias, "B", NamedType("A")),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(result.value.left.value.map(_.message) === Seq("Recursive types aren't supported yet: `A` -> `B` -> `A`"))
  }

  test("a type def referring to itself is an error") {
    val block = Block(Seq(TypeDef(TypeDefKind.Tagged, "Self", NamedType("Self"))))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    assert(result.value.left.value.map(_.message) === Seq("Recursive types aren't supported yet: `Self` -> `Self`"))
  }

  test("type params named like a type def of the block are not references to it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Box", Seq(Ast.TypeParam("A")), NamedType("A")),
      TypeDef(TypeDefKind.Alias, "A", Ast.FuncType(Seq(Ast.TypeParam("Box")), NamedType("Box"), NamedType("Box"))),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs)

    // no false cycle (Box -> A -> Box), and each name resolved to the type param, not to the block's type def
    inside(result.value.value.currentContents) {
      case Seq(Terms.TypeDef(boxCon: TypeCons.WrapperTypeCon), Terms.TypeDef(aCon: TypeCons.WrapperTypeCon)) =>
        assert(boxCon.wrappedType === Types.Param(boxCon.typeParams.head))
        inside(aCon.wrappedType) { case Types.Func(Seq(boxParam), parameter, result) =>
          assert((parameter, result) === ((Types.Param(boxParam), Types.Param(boxParam))))
        }
    }
  }

  test("valueDecl should allow shadowing a value from the parent scope") {
    val parentEnv = Env.empty.addOrShadowValue("value", Types.Uni, NoPos).value.value
    val parentValue = parentEnv.getValue("value").get
    val block = Block(Seq(ValueDecl("value", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs)

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.currentEnv.local.values.get("value").get.defId !== parentValue.defId)
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
    assert(result.value.value.currentEnv.local.typeCons.get("Type").get.defId !== parentType.defId)
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

  test("a singleton used only as a type or only as a value is used") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "AsType"),
      TypeDef(TypeDefKind.Singleton, "AsValue"),
      ValueDecl("typed", NamedType("AsType")),
      Tuple(Seq(NamedValue("typed"), NamedValue("AsValue"))),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Nil)
  }

  test("an unused singleton is reported once") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Unused"),
      UnitLiteral(),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Seq("Unused type `Unused`"))
  }

  test("a wrapper type used only as its constructor is used") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      NamedValue("Id"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block)

    assert(result.log.warnings.map(_.message) === Nil)
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
