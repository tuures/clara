package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, ValueDecl => _, _}
  import AstTestHelpers._

  // the env after the names and types passes, also after their errors
  private def namesAndTypesEnv(block: IndexedBlock): Env =
    // the errors are in the log of the run, the result is only what's kept in spite of them
    NamesPass.walk(Env.empty, block).flatMap { case (namedEnv, _) => TypesPass.walk(namedEnv, block) }.runFrom(AnLog())._1.get

  test("empty Block (program block) should produce unit type with no warning about missing expression") {
    val block = Block(Seq.empty)

    val blockTermAn = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    val expectedTerm = Terms.Block(Seq.empty, Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.warnings.isEmpty)
  }

  test("typeDef: IndexedBlock containing only a typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Block(Seq(typeDef))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).flatMap { endState =>
      BlockAnalyzerImpl.finishBlock(An.valid(endState), block.pos, isProgramBlock = false).map(term => (endState, term))
    }.run
    val (endState, blockTerm) = result.value.value

    val typ = endState.env.getTypeEntry("Unit").collect { case EnvTypeCon(con, _) => con }.get

    inside(typ) {
      case con @ TypeCons.WrapperTypeCon(typeDefKind, name, typeParams, _, _, _) =>
        val structure = result.log.typeStructures.structure(con)
        assert((typeDefKind, name, typeParams, structure) === (TypeDefKind.Alias, "Unit", Nil, Types.Uni))
    }

    assert(blockTerm === Terms.Block(Seq(Terms.TypeDef(typ)), Types.Uni))

    assert(result.log.warnings.map(_.message) === Seq(
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

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

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

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).flatMap { endState =>
      BlockAnalyzerImpl.finishBlock(An.valid(endState), block.pos, isProgramBlock = false).map(term => (endState, term))
    }.run
    val (endState, blockTerm) = result.value.value

    assert(endState.env.getValue("declared").map(_.typ) === Some(Types.Uni))
    assert(endState.env.getValue("defined").map(_.typ) === Some(Types.Uni))

    val expectedTerm = Terms.Block(Seq(
      Terms.ValueDecl("declared"),
      Terms.ValueDef(Terms.CapturePattern("defined", Types.Uni), Nil, Terms.UnitLiteral()),
      Terms.Tuple(
        Seq(Terms.NamedValue("declared", Nil, Types.Uni), Terms.NamedValue("defined", Nil, Types.Uni)),
        Types.Tuple(Seq(Types.Uni, Types.Uni)),
      ),
    ), Types.Tuple(Seq(Types.Uni, Types.Uni)))
    assert(blockTerm === expectedTerm)
    assert(result.log.warnings.isEmpty)
  }

  test("the signature of an annotated value def is the expected type of its value expression") {
    val block = Block(Seq(
      ValueDef(
        PatternAs(CapturePattern("f"), Ast.FuncType(Nil, UnitType(), UnitType())),
        Lambda(CapturePattern("x"), NamedValue("x")),
      ),
      Call(NamedValue("f"), UnitLiteral()),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    // the lambda's parameter has the signature's parameter type
    assert(result.value.value.typ === Types.Uni)
    assert(result.log.warnings.isEmpty)
  }

  // duplicates: only the first declaration of a name in a scope is registered

  test("typeDef should fail when shadowing an existing local type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  test("a value declared twice is a duplicate") {
    val block = Block(Seq(
      ValueDecl("value", UnitType()),
      ValueDecl("value", UnitType()),
      ValueDef(TuplePattern(Seq(CapturePattern("other"), CapturePattern("value"))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    // by a `::declare` and by a value def
    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `value`",
      "Cannot shadow existing value with same name `value`",
    ))
  }

  test("a `::declare` is a duplicate of an earlier value def or `::declare` of the same name") {
    val block = Block(Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      ValueDecl("value", UnitType()),
      ValueDecl("value", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `value`",
      "Cannot shadow existing value with same name `value`",
    ))
  }

  test("an annotated value def is a duplicate of an earlier value def of the same name, annotated or not") {
    val block = Block(Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("value"), UnitType()), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("value"), UnitType()), UnitLiteral()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `value`",
      "Cannot shadow existing value with same name `value`",
    ))
  }

  test("a namespace and a value with the same name in the same scope is an error") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Color"), Nil),
      ValueDef(CapturePattern("Color"), UnitLiteral()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    // TODO: could be Cannot shadow existing namespace...
    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing value with same name `Color`"))
  }

  test("a duplicate `::declare` doesn't define an earlier value def's value") {
    val block = Block(Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      ValueDecl("value", UnitType()),
    ))

    val env = namesAndTypesEnv(IndexedBlock.of(block.bcs))

    inside(env.local.values.get("value")) { case Some(EnvDeclaredValue(_, _, _)) => }
  }

  test("a duplicate annotated value def has a signature of its own, not the earlier value def's") {
    val analyzerBlock = IndexedBlock.of(Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("value"), UnitType()), UnitLiteral()),
    ))
    val Seq(IndexedBlock.ValueDef(_, Seq(first)), IndexedBlock.SingleTypedValueDef(_, duplicate, _)) = analyzerBlock.contents

    val env = namesAndTypesEnv(analyzerBlock)

    assert(env.signature(first.defId) === None)
    assert(env.signature(duplicate.defId) === Some(Types.Uni))
  }

  test("a duplicate's type expressions are still analyzed") {
    val block = Block(Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      ValueDecl("value", NamedType("Unknown")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `value`",
      "Unknown type `Unknown`",
    ))
  }

  test("a duplicate type def's structure is still analyzed, with its own type params") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Pair", Seq(TypeParam("A")), UnitType()),
      TypeDef(TypeDefKind.Tagged, "Pair", Seq(TypeParam("A"), TypeParam("A")), TupleType(Seq(NamedType("A"), NamedType("Unknown")))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `A`",
      "Cannot shadow existing type with same name `Pair`",
      "Unknown type `Unknown`",
    ))
    // only the first one's structure is registered
    assert(result.log.typeStructures.structures.values.toSeq === Seq(Types.Uni))
  }

  test("a value pattern binding a name twice keeps the first one") {
    val block = Block(Seq(
      ValueDef(TuplePattern(Seq(CapturePattern("a"), CapturePattern("a"))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      ValueDecl("a", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `a`",
      "Cannot shadow existing value with same name `a`",
    ))
  }

  test("a namespace and a type with the same name in the same scope is an error") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Nil),
      TypeDef(TypeDefKind.Alias, "Space", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Space`"))
  }

  test("a namespace with the name of a value keeps its name as a type") {
    val block = Block(Seq(
      ValueDecl("Space", UnitType()),
      NamespaceDef(false, NameWithPos("Space"), Nil),
      TypeDef(TypeDefKind.Alias, "Space", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing value with same name `Space`",
      "Cannot shadow existing type with same name `Space`",
    ))
  }

  test("a singleton with the name of a value keeps its type") {
    val block = Block(Seq(
      ValueDecl("Red", UnitType()),
      TypeDef(TypeDefKind.Singleton, "Red"),
      TypeDef(TypeDefKind.Alias, "Uses", NamedType("Red")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing value with same name `Red`"))
  }

  test("a tagged type with the name of a value keeps its constructor") {
    val block = Block(Seq(
      ValueDecl("Id", UnitType()),
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing value with same name `Id`"))
  }

  test("a later declaration of a name is a duplicate also when the first one failed to be registered") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Type", Seq(TypeParam("A"))),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot define type parameters for type `Type`",
      "Cannot shadow existing type with same name `Type`",
    ))
  }

  test("a type def with errors shadows a parent type of the same name") {
    val parentEnv = BlockAnalyzerImpl.walkBlockContents(Env.empty, Seq(TypeDef(TypeDefKind.Alias, "Type", UnitType()))).run.value.value.env
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Type", Seq(TypeParam("A"))),
      TypeDef(TypeDefKind.Alias, "Uses", NamedType("Type")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(parentEnv.startNestedScope, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot define type parameters for type `Type`"))
    inside(result.log.typeStructures.structures.values.toSeq) { case Seq(Types.Singleton(con)) => assert(con.name === "Type") }
  }

  test("the companion of a type def with errors is analyzed") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Type"),
      NamespaceDef(true, NameWithPos("Type"), Seq(
        TypeDef(TypeDefKind.Alias, "Member", UnitType()),
        TypeDef(TypeDefKind.Alias, "Other", NamedType("Member")),
        TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")),
      )),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Structure needs to be defined for type `Type`",
      "Unknown type `Unknown`",
    ))
  }

  test("a type def with errors still defines its type, with its companion and constructor") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Generated"),
      TypeDef(TypeDefKind.Tagged, "Extended"),
      NamespaceDef(true, NameWithPos("Extended"), Seq(TypeDef(TypeDefKind.Alias, "Member", UnitType()))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run
    val env = namesAndTypesEnv(IndexedBlock.of(block.bcs))

    assert(result.value.left.value.map(_.message) === Seq(
      "Structure needs to be defined for type `Generated`",
      "Structure needs to be defined for type `Extended`",
    ))
    for (name <- Seq("Generated", "Extended")) inside(env.local.types.get(name)) { case Some(EnvCompanion(_, nsId, _)) =>
      assert(env.namespaceMembers(nsId).values.get("apply").isDefined)
    }
  }

  test("a type def with duplicate type params leaves the duplicate out") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Pair", Seq(TypeParam("A"), TypeParam("A")), TupleType(Seq(NamedType("A"), NamedType("A")))),
      TypeDef(TypeDefKind.Alias, "Uses", generic("Pair", UnitType())),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `A`"))
  }

  test("the body of a duplicate namespace is analyzed in a namespace of its own") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(TypeDef(TypeDefKind.Alias, "Member", UnitType()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(
        TypeDef(TypeDefKind.Alias, "Member", UnitType()),
        TypeDef(TypeDefKind.Alias, "Other", NamedType("Member")),
        TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")),
      )),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Space`",
      "Unknown type `Unknown`",
    ))
  }

  test("a duplicate type def doesn't analyze the companion of the first one again") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      NamespaceDef(true, NameWithPos("Id"), Seq(TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Id`",
      "Unknown type `Unknown`",
    ))
  }

  test("a later declaration cannot shadow an existing local type even if the first one failed") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", NamedType("Unknown")),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Type`",
      "Unknown type `Unknown`",
    ))
    assert(result.log.typeStructures.structures.values.toSeq === Seq(Types.ErrorType))
  }

  test("the types pass runs also after errors in the names pass, so its errors are reported too") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Other", NamedType("Unknown")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Type`",
      "Unknown type `Unknown`",
    ))
  }

  test("a namespace with errors in its names pass is still analyzed by the types pass") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(
        TypeDef(TypeDefKind.Alias, "Type", NamedType("Unknown")),
        TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      )),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Type`",
      "Unknown type `Unknown`",
    ))
    // the latter one didn't define the first one's structure
    assert(result.log.typeStructures.structures.values.toSeq === Seq(Types.ErrorType))
  }

  test("an error in a namespace's body doesn't lose the namespace or its other members") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(
        TypeDef(TypeDefKind.Alias, "Type", UnitType()),
        TypeDef(TypeDefKind.Alias, "Type", UnitType()),
        TypeDef(TypeDefKind.Alias, "Other", UnitType()),
      )),
      TypeDef(TypeDefKind.Alias, "Uses", Ast.NamedType(Seq(NameWithPos("Space")), NameWithPos("Other"), Nil)),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  test("an error in a companion's body doesn't lose its type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      NamespaceDef(true, NameWithPos("Id"), Seq(
        TypeDef(TypeDefKind.Alias, "Type", UnitType()),
        TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      )),
      TypeDef(TypeDefKind.Alias, "Uses", NamedType("Id")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  test("valueDecl should allow shadowing a value from the parent scope") {
    val parentEnv = Env.empty.addValue("value", Types.Uni, NoPos).run.value.value
    val parentValue = parentEnv.getValue("value").get
    val block = Block(Seq(ValueDecl("value", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs).run

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.env.local.values.get("value").get.defId !== parentValue.defId)
  }

  test("typeDef should allow shadowing a type from the parent scope") {
    val parentTypeDef = TypeDef(TypeDefKind.Alias, "Type", UnitType())
    val parentEnv = BlockAnalyzerImpl.walkBlockContents(Env.empty, Seq(parentTypeDef)).run.value.value.env
    val parentType = parentEnv.getTypeEntry("Type").get
    val block = Block(Seq(TypeDef(TypeDefKind.Alias, "Type", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs).run

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.env.local.types.get("Type").get.defId !== parentType.defId)
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

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Unused value `x`"))
  }

  test("case pattern should allow shadowing an outer value") {
    val block = Block(Seq(
      ValueDef(CapturePattern("x"), UnitLiteral()),
      ValueDef(CapturePattern("f"), Piecewise(Seq(
        PatternAs(CapturePattern("x"), UnitType()) -> NamedValue("x"),
      ))),
      NamedValue("f"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Unused value `x`"))
  }

  test("a case pattern's names in a namespace are local to the case") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(
        ValueDef(CapturePattern("f"), Piecewise(Seq(
          PatternAs(CapturePattern("x"), UnitType()) -> NamedValue("x"),
        ))),
      )),
      MemberSelection(NamedValue("Space"), NamedValue("f")),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    inside(result.value.value.bcs) { case Seq(Terms.ValueDef(_, Seq("Space"), Terms.Piecewise(Seq((_, body)), _)), _) =>
      assert(body === Terms.NamedValue("x", Nil, Types.Uni))
    }
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

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.isRight)
    assert(result.log.warnings.map(_.message) === Seq("Unused type `T`"))
  }

  // the error type: what failed is the error type, so that the values are analyzed after errors without follow-ups

  private def errors(members: BlockContent*) = BlockAnalyzer.regularBlockTerm(Env.empty, Block(members)).run.value.left.value.map(_.message)

  test("the values of a block are analyzed also after errors in its names and types passes") {
    assert(errors(
      TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")),
      TypeDef(TypeDefKind.Alias, "Bad", UnitType()),
      NamedValue("nope"),
    ) === Seq(
      "Cannot shadow existing type with same name `Bad`",
      "Unknown type `Unknown`",
      "Unknown value `nope`",
    ))
  }

  test("a type that failed fits every value and every value's type fits it") {
    assert(errors(
      TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Bad")), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("y"), UnitType()), NamedValue("x")),
      NamedValue("y"),
    ) === Seq("Unknown type `Unknown`"))
  }

  test("a type that failed gives a lambda's parameter its type") {
    assert(errors(
      ValueDef(PatternAs(CapturePattern("f"), NamedType("Unknown")), Lambda(CapturePattern("x"), NamedValue("x"))),
      ValueDef(PatternAs(CapturePattern("g"), Ast.FuncType(Nil, NamedType("Unknown"), UnitType())), Lambda(CapturePattern("x"), NamedValue("x"))),
    ) === Seq("Unknown type `Unknown`", "Unknown type `Unknown`"))
  }

  test("a value with a type annotation that failed reports it once") {
    assert(errors(
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Unknown")), UnitLiteral()),
      NamedValue("x"),
    ) === Seq("Unknown type `Unknown`"))
  }

  test("a value that failed has the error type") {
    assert(errors(
      ValueDef(CapturePattern("x"), NamedValue("nope")),
      Call(NamedValue("x"), UnitLiteral()),
      MemberSelection(NamedValue("x"), NamedValue("member")),
      ValueDef(TuplePattern(Seq(CapturePattern("a"), CapturePattern("b"))), NamedValue("x")),
      Tuple(Seq(NamedValue("a"), NamedValue("b"))),
    ) === Seq("Unknown value `nope`"))
  }

  test("a value def keeps its type annotation when its value fails") {
    assert(errors(
      TypeDef(TypeDefKind.Singleton, "Red"),
      TypeDef(TypeDefKind.Singleton, "Blue"),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Red")), NamedValue("nope")),
      // x was declared with type Red even though the assignment failed due to unknown value `nope`
      ValueDef(PatternAs(CapturePattern("y"), NamedType("Blue")), NamedValue("x")),
      UnitLiteral(),
    ) === Seq("Unknown value `nope`", "Type `Red` is not assignable to type `Blue`"))
  }

  test("a value def keeps its type annotation when its value doesn't fit it") {
    assert(errors(
      TypeDef(TypeDefKind.Singleton, "Red"),
      TypeDef(TypeDefKind.Singleton, "Blue"),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Red")), NamedValue("Blue")),
      // x was declared with type Red even though the assignment failed due to type mismatch
      ValueDef(PatternAs(CapturePattern("y"), NamedType("Blue")), NamedValue("x")),
      UnitLiteral(),
    ) === Seq("Type `Blue` is not assignable to type `Red`", "Type `Red` is not assignable to type `Blue`"))
  }

  test("a pattern that failed defines its names as the error type") {
    assert(errors(
      TypeDef(TypeDefKind.Singleton, "Red"),
      ValueDef(TuplePattern(Seq(CapturePattern("a"), PatternAs(CapturePattern("b"), NamedType("Red")))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      Tuple(Seq(NamedValue("a"), NamedValue("b"))),
    ) === Seq("Type `()` is not assignable to type `Red`"))
  }

  test("a type def with errors can be used as a type and through its constructor") {
    assert(errors(
      TypeDef(TypeDefKind.Tagged, "Id"),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Id")), Call(NamedValue("Id"), Lambda(CapturePattern("a"), NamedValue("a")))),
      NamedValue("x"),
    ) === Seq("Structure needs to be defined for type `Id`"))
  }

  test("a duplicate value def analyzes its value but defines nothing") {
    assert(errors(
      TypeDef(TypeDefKind.Singleton, "Red"),
      ValueDef(CapturePattern("x"), NamedValue("Red")),
      ValueDef(CapturePattern("x"), NamedValue("nope")),
      ValueDef(PatternAs(CapturePattern("y"), NamedType("Red")), NamedValue("x")),
      UnitLiteral(),
    ) === Seq("Cannot shadow existing value with same name `x`", "Unknown value `nope`"))
  }

  test("a duplicate annotated value def reports its annotation once") {
    assert(errors(
      ValueDef(CapturePattern("x"), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Unknown")), UnitLiteral()),
      UnitLiteral(),
    ) === Seq("Cannot shadow existing value with same name `x`", "Unknown type `Unknown`"))
  }

  test("a duplicate annotated value def is checked against its own annotation") {
    assert(errors(
      TypeDef(TypeDefKind.Singleton, "Red"),
      ValueDef(CapturePattern("x"), UnitLiteral()),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Red")), UnitLiteral()),
      UnitLiteral(),
    ) === Seq("Cannot shadow existing value with same name `x`", "Type `()` is not assignable to type `Red`"))
  }

  test("the values of a duplicate namespace are analyzed in a namespace of its own") {
    assert(errors(
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDef(CapturePattern("x"), UnitLiteral()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(
        ValueDef(CapturePattern("x"), UnitLiteral()),
        ValueDef(CapturePattern("y"), NamedValue("x")),
        ValueDef(CapturePattern("z"), NamedValue("nope")),
      )),
      UnitLiteral(),
    ) === Seq("Cannot shadow existing type with same name `Space`", "Unknown value `nope`"))
  }

  test("the values of each ::extend of a type are analyzed at its position") {
    assert(errors(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      NamespaceDef(true, NameWithPos("Id"), Seq(ValueDef(CapturePattern("x"), NamedValue("later")))),
      NamespaceDef(true, NameWithPos("Id"), Seq(ValueDef(CapturePattern("later"), UnitLiteral()))),
      UnitLiteral(),
    ) === Seq("`later` is used before its definition"))
  }

  test("the values of an ::extend without a type aren't analyzed") {
    assert(errors(
      NamespaceDef(true, NameWithPos("Missing"), Seq(ValueDef(CapturePattern("x"), NamedValue("nope")))),
      UnitLiteral(),
    ) === Seq("Cannot extend `Missing`, there's no type `Missing` defined in this scope"))
  }

  test("an error in a nested block is the error type outside of it") {
    assert(errors(
      ValueDef(CapturePattern("x"), Block(Seq(
        TypeDef(TypeDefKind.Alias, "Bad", NamedType("Unknown")),
        UnitLiteral(),
      ))),
      // no follow-up error from the invalid call as x has the error type
      Call(NamedValue("x"), UnitLiteral()),
    ) === Seq("Unknown type `Unknown`"))
  }

  test("the values of a block aren't analyzed after an unsupported recursive type as the checks wouldn't terminate") {
    assert(errors(
      TypeDef(TypeDefKind.Alias, "Loop", TupleType(Seq(UnitType(), NamedType("Loop")))),
      ValueDef(PatternAs(CapturePattern("x"), NamedType("Loop")), UnitLiteral()),
      NamedValue("nope"),
    ) === Seq("Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: `Loop` -> `Loop`"))
  }

  // type def kinds

  test("an opaque type or a singleton with a structure is an error") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Opaque, "Opaque", Seq(TypeParam("A")), UnitType()),
      TypeDef(TypeDefKind.Singleton, "Single", UnitType()),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot define structure for type `Opaque`",
      "Cannot define structure for type `Single`",
    ))
  }

  // values: the names pass declares them, the types pass defines the `::declare`d ones, the values pass the others

  test("the names pass declares the names a value def binds") {
    val block = Block(Seq(
      ValueDef(TuplePattern(Seq(CapturePattern("a"), PatternAs(CapturePattern("b"), UnitType()))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDef(CapturePattern("member"), UnitLiteral()))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    val (env, _) = result.value.value
    for (name <- Seq("a", "b")) inside(env.local.values.get(name)) { case Some(_: EnvDeclaredValue) => }
    inside(env.local.values.get("Space")) { case Some(EnvNamespaceRef(spaceNsId, _)) =>
      inside(env.namespaceMembers(spaceNsId).values.get("member")) { case Some(_: EnvDeclaredValue) => }
    }
  }

  test("the types pass defines a `::declare`d value by its type, keeping its DefId") {
    val block = Block(Seq(
      ValueDecl("declared", NamedType("Below")),
      TypeDef(TypeDefKind.Singleton, "Below"),
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDecl("member", UnitType()))),
    ))
    val analyzerBlock = IndexedBlock.of(block.bcs)

    val result = NamesPass.walk(Env.empty, analyzerBlock).flatMap { case (namedEnv, _) =>
      TypesPass.walk(namedEnv, analyzerBlock).map(typedEnv => (namedEnv, typedEnv))
    }.run

    val (namedEnv, typedEnv) = result.value.value
    inside((namedEnv.local.values.get("declared"), typedEnv.local.values.get("declared"))) {
      case (Some(EnvDeclaredValue(declaredId, _, _)), Some(EnvValue(Types.Singleton(belowCon), definedId, Nil, _))) =>
        assert(belowCon.name === "Below")
        assert(definedId === declaredId)
    }
    inside(typedEnv.local.values.get("Space")) { case Some(EnvNamespaceRef(spaceNsId, _)) =>
      inside(typedEnv.namespaceMembers(spaceNsId).values.get("member")) {
        case Some(EnvValue(Types.Uni, _, Seq("Space"), _)) =>
      }
    }
  }

  test("the types pass gives a value def a signature from its top-level annotation only") {
    val analyzerBlock = IndexedBlock.of(Seq(
      ValueDef(PatternAs(CapturePattern("signed"), NamedType("Below")), NamedValue("Below")),
      TypeDef(TypeDefKind.Singleton, "Below"),
      ValueDef(TuplePattern(Seq(PatternAs(CapturePattern("a"), UnitType()), CapturePattern("b"))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDef(PatternAs(CapturePattern("member"), UnitType()), UnitLiteral()))),
    ))
    val Seq(
      IndexedBlock.SingleTypedValueDef(_, signed, _),
      _,
      IndexedBlock.ValueDef(_, tupleNames),
      IndexedBlock.Namespace(_, _, IndexedBlock.NamespaceBody(_, _, IndexedBlock(Seq(IndexedBlock.SingleTypedValueDef(_, member, _))))),
    ) = analyzerBlock.contents

    val (env, _) = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, analyzerBlock).run.value.value

    assert(env.signature(signed.defId).map(Types.toSource) === Some("Below"))
    // the annotations inside other patterns are checked by the values pass
    assert(tupleNames.map(name => env.signature(name.defId)) === Seq(None, None))
    assert(env.signature(member.defId) === Some(Types.Uni))
  }

  // forward references: the names pass registers every type before the types pass analyzes what they wrap, and the
  // types of a block are analyzed before its values

  test("a type def can refer to a type defined below it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Pair", TupleType(Seq(NamedType("Below"), NamedType("Below")))),
      TypeDef(TypeDefKind.Singleton, "Below"),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(pairCon: TypeCons.WrapperTypeCon), Terms.TypeDef(belowCon: TypeCons.SingletonTypeCon))) =>
        val below = Types.Singleton(belowCon)
        assert(result.log.typeStructures.structure(pairCon) === Types.Tuple(Seq(below, below)))
    }
  }

  test("a value is used before its definition") {
    val block = Block(Seq(
      ValueDef(CapturePattern("early"), NamedValue("late")),
      ValueDef(CapturePattern("late"), UnitLiteral()),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`late` is used before its definition"))
  }

  test("an annotated value def can't be used before its definition either") {
    val block = Block(Seq(
      ValueDef(CapturePattern("early"), NamedValue("late")),
      ValueDef(PatternAs(CapturePattern("late"), UnitType()), UnitLiteral()),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`late` is used before its definition"))
  }

  private def valueDefFuncUniUni(name: String, body: ValueExpr) =
    ValueDef(PatternAs(CapturePattern(name), Ast.FuncType(Nil, UnitType(), UnitType())), Lambda(UnitPattern(), body))
  private def call(name: String) = Call(NamedValue(name), UnitLiteral())

  test("the function runs of a block are its consecutive function defs") {
    val block = Block(Seq(
      valueDefFuncUniUni("a", UnitLiteral()),
      ValueDef(CapturePattern("b"), Piecewise(Seq(UnitPattern() -> UnitLiteral()))),
      ValueDef(CapturePattern("value"), UnitLiteral()),
      valueDefFuncUniUni("c", UnitLiteral()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      valueDefFuncUniUni("d", UnitLiteral()),
    ))

    assert(ValuesPass.functionRuns(block.bcs) === Seq(Seq("a", "b"), Seq("a", "b"), Nil, Seq("c"), Nil, Seq("d")))
  }

  test("functions defined one after another can refer to each other before their definitions") {
    val block = Block(Seq(
      valueDefFuncUniUni("ping", call("pong")),
      valueDefFuncUniUni("pong", call("ping")),
      call("ping"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.typ === Types.Uni)
    // the use before its definition counts
    assert(result.log.warnings.isEmpty)
  }

  test("a function can refer to itself") {
    val block = Block(Seq(
      valueDefFuncUniUni("loop", call("loop")),
      call("loop"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.typ === Types.Uni)
  }

  test("the functions of a namespace can refer to each other before their definitions, also qualified") {
    val block = Block(Seq(
      color(
        valueDefFuncUniUni("ping", Call(colorMember("pong"), UnitLiteral())),
        valueDefFuncUniUni("pong", call("ping")),
      ),
      Call(colorMember("ping"), UnitLiteral()),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.typ === Types.Uni)
    assert(result.log.warnings.isEmpty)
  }

  test("a function can't refer to a function defined after a value in between") {
    val block = Block(Seq(
      valueDefFuncUniUni("early", call("late")),
      ValueDef(CapturePattern("between"), UnitLiteral()),
      valueDefFuncUniUni("late", UnitLiteral()),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`late` is used before its definition"))
  }

  test("a function referred to before its definition needs a signature") {
    val block = Block(Seq(
      valueDefFuncUniUni("early", call("late")),
      ValueDef(CapturePattern("late"), Lambda(UnitPattern(), UnitLiteral())),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`late` needs a type annotation to be used before its definition"))
  }

  test("a `::declare`d value can be used before its declaration") {
    val block = Block(Seq(
      ValueDef(CapturePattern("early"), NamedValue("declared")),
      ValueDecl("declared", UnitType()),
      NamedValue("early"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.typ === Types.Uni)
    assert(result.log.warnings.isEmpty)
  }

  test("a value of a block shadows an outer one also before its definition") {
    val block = Block(Seq(
      ValueDef(CapturePattern("x"), UnitLiteral()),
      Block(Seq(
        ValueDef(CapturePattern("y"), NamedValue("x")),
        ValueDef(CapturePattern("x"), UnitLiteral()),
      )),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`x` is used before its definition"))
  }

  test("a value can refer to a type defined below it") {
    val block = Block(Seq(
      ValueDecl("x", NamedType("Below")),
      TypeDef(TypeDefKind.Alias, "Below", UnitType()),
      NamedValue("x"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    inside(result.value.value) { case Terms.Block(Seq(Terms.TypeDef(belowCon: TypeCons.WrapperTypeCon), _*), typ) =>
      assert(typ === Types.Alias(belowCon, Nil))
    }
    assert(result.log.warnings.isEmpty)
  }

  test("type defs come first in the block term, in source order") {
    val block = Block(Seq(
      ValueDecl("x", NamedType("Dependent")),
      TypeDef(TypeDefKind.Alias, "Dependent", NamedType("Dependency")),
      NamedValue("x"),
      TypeDef(TypeDefKind.Singleton, "Dependency"),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.value.contents.map(_.getClass.getSimpleName) === Seq("TypeDef", "TypeDef", "ValueDecl", "NamedValue"))
    assert(result.value.value.contents.collect { case Terms.TypeDef(con) => con.name } === Seq("Dependent", "Dependency"))
  }

  // recursion

  private val a = Seq(Ast.TypeParam("A"))
  private def generic(name: String, typeArg: TypeExpr) = NamedType(name, Seq(typeArg))

  test("a boxed type can refer to itself directly") {
    // ::boxed Count: () | Count, each step is a box
    val block = Block(Seq(
      TypeDef(TypeDefKind.Boxed, "Count", UnionType(Seq(UnitType(), NamedType("Count")))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("a tagged type can refer to itself inside a tuple") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Chain", UnionType(Seq(UnitType(), TupleType(Seq(UnitType(), NamedType("Chain")))))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("an alias and a boxed type can refer to each other") {
    // ::alias Tree<A>: A | Branch<A>, ::boxed Branch<A>: (Tree<A>, Tree<A>)
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Tree", a, UnionType(Seq(NamedType("A"), generic("Branch", NamedType("A"))))),
      TypeDef(TypeDefKind.Boxed, "Branch", a, TupleType(Seq(generic("Tree", NamedType("A")), generic("Tree", NamedType("A"))))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("a recursive type can refer to itself with its own type parameters as type arguments") {
    // ::boxed List<A>: () | (A, List<A>)
    val block = Block(Seq(
      TypeDef(TypeDefKind.Boxed, "List", a, UnionType(Seq(UnitType(), TupleType(Seq(NamedType("A"), generic("List", NamedType("A"))))))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("recursive aliases are an error") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "A", TupleType(Seq(NamedType("B"), UnitType()))),
      TypeDef(TypeDefKind.Alias, "B", NamedType("A")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: `A` -> `B` -> `A`"
    ))
  }

  test("a recursive type referring to itself directly is an error") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Self", UnionType(Seq(NamedType("Self"), UnitType()))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: `Self` -> `Self`"
    ))
  }

  test("a recursive type referring to itself with other type arguments than type parameters is an error") {
    // ::boxed Nest<A>: () | (A, Nest<(A, A)>)
    val block = Block(Seq(
      TypeDef(TypeDefKind.Boxed, "Nest", a,
        UnionType(Seq(UnitType(), TupleType(Seq(NamedType("A"), generic("Nest", TupleType(Seq(NamedType("A"), NamedType("A"))))))))
      ),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "A recursive type can only refer to itself with type parameters as type arguments, not `Nest<(A, A)>`"
    ))
  }

  test("each group of recursive types is checked on its own, with an error for each rule it breaks") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Loop", NamedType("Loop")),
      TypeDef(TypeDefKind.Tagged, "Self", NamedType("Self")),
      TypeDef(TypeDefKind.Boxed, "Fine", UnionType(Seq(UnitType(), NamedType("Fine")))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    // `Loop` breaks two rules
    assert(result.value.left.value.map(_.message) === Seq(
      "Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: `Loop` -> `Loop`",
      "A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: `Loop` -> `Loop`",
      "A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: `Self` -> `Self`",
    ))
  }

  test("type params named like a type def of the block are not references to it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Box", Seq(Ast.TypeParam("A")), NamedType("A")),
      TypeDef(TypeDefKind.Alias, "A", Ast.FuncType(Seq(Ast.TypeParam("Box")), NamedType("Box"), NamedType("Box"))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    // no false cycle (Box -> A -> Box), and each name resolved to the type param, not to the block's type def
    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(boxCon: TypeCons.WrapperTypeCon), Terms.TypeDef(aCon: TypeCons.WrapperTypeCon))) =>
        assert(result.log.typeStructures.structure(boxCon) === Types.Param(boxCon.typeParams.head))
        inside(result.log.typeStructures.structure(aCon)) { case Types.Func(Seq(boxParam), parameter, result) =>
          assert((parameter, result) === ((Types.Param(boxParam), Types.Param(boxParam))))
        }
    }
  }

  // namespaces: types and singletons are registered with the type defs, values at the namespace's position

  private def color(members: InBlockDecl*) = NamespaceDef(false, NameWithPos("Color"), members)
  private def colorMember(name: String) = MemberSelection(NamedValue("Color"), NamedValue(name))

  test("namespace members are qualified outside the namespace and unqualified inside it") {
    val block = Block(Seq(
      color(
        TypeDef(TypeDefKind.Singleton, "Red"),
        ValueDef(CapturePattern("favorite"), NamedValue("Red")),
      ),
      colorMember("favorite"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    inside(result.value.value.bcs) {
      case Seq(
        Terms.TypeDef(redCon: TypeCons.SingletonTypeCon),
        Terms.ValueDef(_, Seq("Color"), insideRef),
        outsideRef,
      ) =>
        val red = Types.Singleton(redCon)
        assert(redCon.definedInNs === Seq("Color"))
        assert(insideRef === Terms.NamedValue("Red", Seq("Color"), red))
        assert(outsideRef === Terms.NamedValue("favorite", Seq("Color"), red))
    }
    assert(result.log.warnings.isEmpty)
  }

  test("a namespace's values can only be used after its position") {
    val block = Block(Seq(
      colorMember("favorite"),
      color(ValueDef(CapturePattern("favorite"), UnitLiteral())),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`Color.favorite` is used before its definition"))
  }

  test("a namespace's `::declare`d values can be used before its position") {
    val block = Block(Seq(
      colorMember("dark"),
      color(ValueDecl("dark", UnitType())),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.value.contents.head === Terms.NamedValue("dark", Seq("Color"), Types.Uni))
  }

  test("an error in a namespace's value doesn't lose its other values") {
    val block = Block(Seq(
      color(
        ValueDef(CapturePattern("broken"), NamedValue("unknown")),
        ValueDef(CapturePattern("fine"), UnitLiteral()),
      ),
      colorMember("fine"),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Unknown value `unknown`"))
  }

  test("a namespace's members are reachable qualified inside it and inside its nested namespaces") {
    val block = Block(Seq(
      color(
        ValueDef(CapturePattern("a"), UnitLiteral()),
        ValueDef(CapturePattern("b"), colorMember("a")),
        NamespaceDef(false, NameWithPos("Shades"), Seq(ValueDef(CapturePattern("c"), colorMember("b")))),
      ),
      MemberSelection(colorMember("Shades"), NamedValue("c")),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    inside(result.value.value.bcs) {
      case Seq(_, Terms.ValueDef(_, _, bRef), Terms.ValueDef(_, _, cRef), _) =>
        assert(bRef === Terms.NamedValue("a", Seq("Color"), Types.Uni))
        assert(cRef === Terms.NamedValue("b", Seq("Color"), Types.Uni))
    }
  }

  test("a member of a namespace member that is a value is selected from the value") {
    val block = Block(Seq(
      color(ValueDef(CapturePattern("favorite"), Record(Seq(FieldDef("a", None, UnitLiteral()))))),
      MemberSelection(colorMember("favorite"), NamedValue("a")),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.typ === Types.Uni)
  }

  test("an unknown namespace member is reported once also when selecting from it") {
    val block = Block(Seq(
      color(),
      MemberSelection(colorMember("nope"), NamedValue("a")),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("`nope` is not a member of namespace `Color`"))
  }

  test("a type def can refer to a namespace's types declared below it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "AnyColor", Ast.NamedType(Seq(NameWithPos("Color")), NameWithPos("Red"), Nil)),
      color(TypeDef(TypeDefKind.Singleton, "Red")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(anyCon: TypeCons.WrapperTypeCon), Terms.TypeDef(redCon: TypeCons.SingletonTypeCon))) =>
        assert(result.log.typeStructures.structure(anyCon) === Types.Singleton(redCon))
    }
  }

  test("an unused namespace is reported once") {
    val block = Block(Seq(color(), UnitLiteral()))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Seq("Unused namespace `Color`"))
  }

  test("the unused members of a used namespace are reported with their qualified names") {
    val block = Block(Seq(
      color(
        TypeDef(TypeDefKind.Singleton, "Red"),
        ValueDef(CapturePattern("favorite"), UnitLiteral()),
        ValueDef(CapturePattern("unused"), UnitLiteral()),
        NamespaceDef(false, NameWithPos("Shades"), Seq(ValueDef(CapturePattern("dark"), UnitLiteral()))),
      ),
      colorMember("favorite"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    // as a set, the warnings are sorted by position and these have none
    assert(result.log.warnings.map(_.message).toSet === Set(
      "Unused type `Color.Red`",
      "Unused value `Color.unused`",
      // an unused namespace is reported alone, not its members
      "Unused namespace `Color.Shades`",
    ))
  }

  // companions: `::extend X` is the namespace of the type X defined in the same scope

  private def extend(name: String, members: InBlockDecl*) = NamespaceDef(true, NameWithPos(name), members)

  private def qualifiedType(namespace: String, name: String) = Ast.NamedType(Seq(NameWithPos(namespace)), NameWithPos(name), Nil)

  test("a type can refer to its companion's types, before or after the ::extend") {
    val block = Block(Seq(
      extend("Color", TypeDef(TypeDefKind.Singleton, "Red")),
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Red")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(colorCon: TypeCons.WrapperTypeCon), Terms.TypeDef(redCon: TypeCons.SingletonTypeCon))) =>
        assert(redCon.definedInNs === Seq("Color"))
        assert(result.log.typeStructures.structure(colorCon) === Types.Singleton(redCon))
    }
  }

  test("::extend needs a type of the same name in the same scope") {
    val block = Block(Seq(extend("Missing"), extend("Missing")))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot extend `Missing`, there's no type `Missing` defined in this scope",
      "Cannot extend `Missing`, there's no type `Missing` defined in this scope",
    ))
  }

  test("the ::extends of a type are one companion, the type can be before, between or after them") {
    val block = Block(Seq(
      extend("Id", TypeDef(TypeDefKind.Alias, "First", NamedType("Second"))),
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id", TypeDef(TypeDefKind.Alias, "Second", NamedType("Third"))),
      extend("Id", TypeDef(TypeDefKind.Alias, "Third", UnitType())),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    val (env, terms) = result.value.value
    inside(env.local.types.get("Id")) { case Some(EnvCompanion(_, nsId, _)) =>
      assert(env.namespaceMembers(nsId).types.entries.map(_._1).toSet === Set("First", "Second", "Third"))
    }
    // in source order, the companion's members after the type
    assert(terms.collect { case Terms.TypeDef(con) => con.name } === Seq("Id", "First", "Second", "Third"))
  }

  test("the ::extends of a type are one block, a type in one can be extended in another") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Outer", UnitType()),
      extend("Outer", TypeDef(TypeDefKind.Tagged, "Inner", UnitType())),
      extend("Outer", extend("Inner", TypeDef(TypeDefKind.Alias, "Member", UnitType()))),
      TypeDef(TypeDefKind.Alias, "Uses", Ast.NamedType(Seq(NameWithPos("Outer"), NameWithPos("Inner")), NameWithPos("Member"), Nil)),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("a name in two ::extends of a type is a duplicate") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id", TypeDef(TypeDefKind.Alias, "Member", UnitType()), ValueDef(CapturePattern("value"), UnitLiteral())),
      extend("Id", TypeDef(TypeDefKind.Alias, "Member", UnitType()), ValueDef(CapturePattern("value"), UnitLiteral())),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Member`",
      "Cannot shadow existing value with same name `value`",
    ))
  }

  test("an apply in a later ::extend of a type is still its constructor") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id"),
      extend("Id", ValueDef(CapturePattern("apply"), UnitLiteral())),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("`apply` of `Id` is its constructor, it can't be redefined yet"))
  }

  test("an ::extend without its type doesn't extend a namespace of the same name") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(TypeDef(TypeDefKind.Alias, "Inner", UnitType()))),
      extend("Space", TypeDef(TypeDefKind.Alias, "Inner", TupleType(Seq(UnitType(), UnitType())))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot extend `Space`, there's no type `Space` defined in this scope"))
    // the namespace's type isn't given the structure of the ::extend's type of the same name
    assert(result.log.typeStructures.structures.values.toSeq === Seq(Types.Uni))
  }

  test("extending a singleton isn't supported yet") {
    val block = Block(Seq(TypeDef(TypeDefKind.Singleton, "Foo"), extend("Foo")))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Extending a singleton isn't supported yet"))
  }

  test("extending a singleton keeps the singleton") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Foo"),
      extend("Foo"),
      TypeDef(TypeDefKind.Alias, "Uses", NamedType("Foo")),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq("Extending a singleton isn't supported yet"))
  }

  test("the body of a companion that failed to be registered is still checked") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Foo"),
      extend("Foo", TypeDef(TypeDefKind.Alias, "Inner", UnitType()), TypeDef(TypeDefKind.Alias, "Inner", UnitType())),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    // the companion is walked at its type, the `::extend` is checked at its own position
    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Inner`",
      "Extending a singleton isn't supported yet",
    ))
  }

  test("a companion's type can refer to the type itself when that isn't recursive") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Red")),
      extend("Color", TypeDef(TypeDefKind.Singleton, "Red"), TypeDef(TypeDefKind.Alias, "Again", NamedType("Color"))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.isRight)
  }

  test("an alias and its companion's alias can't refer to each other") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Again")),
      extend("Color", TypeDef(TypeDefKind.Alias, "Again", NamedType("Color"))),
    ))

    val result = BlockAnalyzerImpl.analyzeDeclarations(Env.empty, IndexedBlock.of(block.bcs)).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Recursive aliases aren't supported, the recursion needs to go through a tagged or boxed type: `Color` -> `Again` -> `Color`",
      "A recursive type can only refer to itself inside a tuple, a record, a function or a boxed type: `Color` -> `Again` -> `Color`",
    ))
  }

  test("a tagged type's companion can't redefine its constructor apply yet, however it's defined") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id", ValueDef(CapturePattern("apply"), UnitLiteral())),
      TypeDef(TypeDefKind.Boxed, "Box", UnitType()),
      extend("Box", ValueDef(TuplePattern(Seq(CapturePattern("apply"), WildcardPattern())), Tuple(Seq(UnitLiteral(), UnitLiteral())))),
    ))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "`apply` of `Id` is its constructor, it can't be redefined yet",
      "`apply` of `Box` is its constructor, it can't be redefined yet",
    ))
  }

  test("a companion's members are reachable qualified inside it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Opaque, "Token"),
      extend("Token",
        ValueDef(CapturePattern("prefix"), UnitLiteral()),
        ValueDef(CapturePattern("again"), MemberSelection(NamedValue("Token"), NamedValue("prefix"))),
      ),
      MemberSelection(NamedValue("Token"), NamedValue("again")),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.value.value.bcs.last === Terms.NamedValue("again", Seq("Token"), Types.Uni))
  }

  test("an unused type with a companion is reported once, as a type") {
    val block = Block(Seq(extend("Id"), TypeDef(TypeDefKind.Tagged, "Id", UnitType()), UnitLiteral()))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Seq("Unused type `Id`"))
  }

  test("the unused members of a used type's companion are reported with their qualified names") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id", ValueDef(CapturePattern("helper"), UnitLiteral())),
      NamedValue("Id"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    // the type is used through its constructor, its companion's generated `apply`
    assert(result.log.warnings.map(_.message) === Seq("Unused value `Id.helper`"))
  }

  test("block warns about unused local values") {
    val block = Block(Seq(
      ValueDecl("unused", UnitType()),
      ValueDecl("used", UnitType()),
      NamedValue("used"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Seq("Unused value `unused`"))
  }

  test("block warns about unused local types") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Unused", UnitType()),
      TypeDef(TypeDefKind.Alias, "Used", UnitType()),
      ValueDecl("typed", NamedType("Used")),
      NamedValue("typed"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Seq("Unused type `Unused`"))
  }

  test("a singleton used only as a type or only as a value is used") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "AsType"),
      TypeDef(TypeDefKind.Singleton, "AsValue"),
      ValueDecl("typed", NamedType("AsType")),
      Tuple(Seq(NamedValue("typed"), NamedValue("AsValue"))),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Nil)
  }

  test("an unused singleton is reported once") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Unused"),
      UnitLiteral(),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Seq("Unused type `Unused`"))
  }

  test("a wrapper type used only as its constructor is used") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      NamedValue("Id"),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    assert(result.log.warnings.map(_.message) === Nil)
  }

  test("use in a nested block counts as use of the parent block local") {
    val outer = Block(Seq(
      ValueDecl("x", UnitType()),
      Block(Seq(NamedValue("x"))),
    ))

    val result = BlockAnalyzer.regularBlockTerm(Env.empty, outer).run

    assert(result.log.warnings.isEmpty)
  }

  test("program block: Non-unit last expression should warn about discarded value") {
    def lambdaAst = Lambda(Nil, UnitPattern(), UnitLiteral())
    def lambdaTerm = Terms.Lambda(Terms.UnitPattern(), Terms.UnitLiteral(), Types.Func(Nil, Types.Uni, Types.Uni))

    val block = Block(Seq(lambdaAst, lambdaAst))

    val blockTermAn = BlockAnalyzer.programBlockTerm(Env.empty, block).run

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

    val blockTermAn = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

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
