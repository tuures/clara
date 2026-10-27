package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class BlockAnalyzerSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, ValueDecl => _, _}
  import AstTestHelpers._

  test("empty Block (program block) should produce unit type with no warning about missing expression") {
    val block = Block(Seq.empty)

    val blockTermAn = BlockAnalyzer.regularBlockTerm(Env.empty, block).run

    val expectedTerm = Terms.Block(Seq.empty, Types.Uni)
    assert(blockTermAn.value.value === expectedTerm)

    assert(blockTermAn.log.warnings.isEmpty)
  }

  test("typeDef: Block containing only a typeDef should give a warning of missing expression. " +
    "Type definitions should affect the type env inside the block and match the returned term contents.") {
    val typeDef = TypeDef(TypeDefKind.Alias, "Unit", UnitType())
    val block = Block(Seq(typeDef))

    val result = BlockAnalyzerImpl.walkBlockContents(Env.empty, block.bcs).flatMap { endState =>
      BlockAnalyzerImpl.finishBlock(An.valid(endState), block.pos, isProgramBlock = false).map(term => (endState, term))
    }.run
    val (endState, blockTerm) = result.value.value

    val typ = endState.currentEnv.getTypeEntry("Unit").collect { case EnvTypeCon(con, _) => con }.get

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

    assert(endState.currentEnv.getValue("declared").map(_.typ) === Some(Types.Uni))
    assert(endState.currentEnv.getValue("defined").map(_.typ) === Some(Types.Uni))

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

  // forward references: the types of the block are analyzed before its values (see ScopeAnalyzerSpec)

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

    assert(result.value.value.currentContents.map(_.getClass.getSimpleName) === Seq("TypeDef", "TypeDef", "ValueDecl", "NamedValue"))
    assert(result.value.value.currentContents.collect { case Terms.TypeDef(con) => con.name } === Seq("Dependent", "Dependency"))
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

    assert(result.value.value.currentContents.head === Terms.NamedValue("dark", Seq("Color"), Types.Uni))
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

  test("valueDecl should allow shadowing a value from the parent scope") {
    val parentEnv = Env.empty.addValue("value", Types.Uni, NoPos).run.value.value
    val parentValue = parentEnv.getValue("value").get
    val block = Block(Seq(ValueDecl("value", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs).run

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.currentEnv.local.values.get("value").get.defId !== parentValue.defId)
  }

  test("typeDef should allow shadowing a type from the parent scope") {
    val parentTypeDef = TypeDef(TypeDefKind.Alias, "Type", UnitType())
    val parentEnv = TypeDefAnalyzer.typeCon(Env.empty, parentTypeDef).
      flatMap(TypeDefAnalyzer.registerName(Env.empty, parentTypeDef, _)).run.value.value
    val parentType = parentEnv.getTypeEntry("Type").get
    val block = Block(Seq(TypeDef(TypeDefKind.Alias, "Type", UnitType())))

    val result = BlockAnalyzerImpl.walkBlockContents(parentEnv, block.bcs).run

    assert(result.value.isRight)
    assert(result.log.warnings.isEmpty)
    assert(result.value.value.currentEnv.local.types.get("Type").get.defId !== parentType.defId)
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
