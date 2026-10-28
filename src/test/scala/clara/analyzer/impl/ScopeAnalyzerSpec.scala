package clara.analyzer.impl

import clara.ast.Ast
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

// the names and types passes of a scope, see BlockAnalyzerSpec for the values pass and the block as a whole
class ScopeAnalyzerSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, ValueDecl => _, _}
  import AstTestHelpers._

  // duplicates: only the first declaration of a name in a scope is registered

  test("typeDef should fail when shadowing an existing local type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  test("a value declared twice is a duplicate") {
    val block = Block(Seq(
      ValueDecl("value", UnitType()),
      ValueDecl("value", UnitType()),
      ValueDef(TuplePattern(Seq(CapturePattern("other"), CapturePattern("value"))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    // TODO: could be Cannot shadow existing namespace...
    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing value with same name `Color`"))
  }

  test("a later declaration of a name is a duplicate also when the first one failed to be registered") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Type", Seq(TypeParam("A"))),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot define type parameters for type `Type`",
      "Cannot shadow existing type with same name `Type`",
    ))
  }

  test("a later declaration cannot shadow an existing local type even if the first one failed") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", NamedType("Unknown")),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Type`",
      "Unknown type `Unknown`",
    ))
    assert(result.log.typeStructures.structures.isEmpty)
  }

  test("the types pass runs also after errors in the names pass, so its errors are reported too") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Type", UnitType()),
      TypeDef(TypeDefKind.Alias, "Other", NamedType("Unknown")),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot shadow existing type with same name `Type`",
      "Unknown type `Unknown`",
    ))
    // the latter one didn't define the first one's structure
    assert(result.log.typeStructures.structures.isEmpty)
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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot shadow existing type with same name `Type`"))
  }

  // values: the names pass declares them, the types pass defines the `::declare`d ones, the values pass the others

  test("the names pass declares the names a value def binds") {
    val block = Block(Seq(
      ValueDef(TuplePattern(Seq(CapturePattern("a"), PatternAs(CapturePattern("b"), UnitType()))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDef(CapturePattern("member"), UnitLiteral()))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

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
    val impl = ScopeAnalyzerImpl(block.bcs)

    val result = impl.registerNames(Env.empty).flatMap { case (namedEnv, _) =>
      impl.registerTypes(namedEnv).map(typedEnv => (namedEnv, typedEnv))
    }.run

    val (namedEnv, typedEnv) = result.value.value
    inside((namedEnv.local.values.get("declared"), typedEnv.local.values.get("declared"))) {
      case (Some(EnvDeclaredValue(declaredId, None, _, _)), Some(EnvValue(Types.Singleton(belowCon), definedId, Nil, _))) =>
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
    val block = Block(Seq(
      ValueDef(PatternAs(CapturePattern("signed"), NamedType("Below")), NamedValue("Below")),
      TypeDef(TypeDefKind.Singleton, "Below"),
      ValueDef(TuplePattern(Seq(PatternAs(CapturePattern("a"), UnitType()), CapturePattern("b"))), Tuple(Seq(UnitLiteral(), UnitLiteral()))),
      NamespaceDef(false, NameWithPos("Space"), Seq(ValueDef(PatternAs(CapturePattern("member"), UnitType()), UnitLiteral()))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    val (env, _) = result.value.value
    inside(env.local.values.get("signed")) { case Some(EnvDeclaredValue(_, Some(Types.Singleton(belowCon)), _, _)) =>
      assert(belowCon.name === "Below")
    }
    // the annotations inside other patterns are checked by the values pass
    for (name <- Seq("a", "b")) inside(env.local.values.get(name)) { case Some(EnvDeclaredValue(_, None, _, _)) => }
    inside(env.local.values.get("Space")) { case Some(EnvNamespaceRef(spaceNsId, _)) =>
      inside(env.namespaceMembers(spaceNsId).values.get("member")) {
        case Some(EnvDeclaredValue(_, Some(Types.Uni), Seq("Space"), _)) =>
      }
    }
  }

  // forward references: the names pass registers every type before the types pass analyzes what they wrap

  test("a type def can refer to a type defined below it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Pair", TupleType(Seq(NamedType("Below"), NamedType("Below")))),
      TypeDef(TypeDefKind.Singleton, "Below"),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(pairCon: TypeCons.WrapperTypeCon), Terms.TypeDef(belowCon: TypeCons.SingletonTypeCon))) =>
        val below = Types.Singleton(belowCon)
        assert(result.log.typeStructures.structure(pairCon) === Types.Tuple(Seq(below, below)))
    }
  }

  // recursion

  test("recursive type defs are an error") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "A", TupleType(Seq(NamedType("B"), UnitType()))),
      TypeDef(TypeDefKind.Alias, "B", NamedType("A")),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Recursive types aren't supported yet: `A` -> `B` -> `A`"))
  }

  test("a type def referring to itself is an error") {
    val block = Block(Seq(TypeDef(TypeDefKind.Tagged, "Self", NamedType("Self"))))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Recursive types aren't supported yet: `Self` -> `Self`"))
  }

  test("type params named like a type def of the block are not references to it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Tagged, "Box", Seq(Ast.TypeParam("A")), NamedType("A")),
      TypeDef(TypeDefKind.Alias, "A", Ast.FuncType(Seq(Ast.TypeParam("Box")), NamedType("Box"), NamedType("Box"))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    // no false cycle (Box -> A -> Box), and each name resolved to the type param, not to the block's type def
    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(boxCon: TypeCons.WrapperTypeCon), Terms.TypeDef(aCon: TypeCons.WrapperTypeCon))) =>
        assert(result.log.typeStructures.structure(boxCon) === Types.Param(boxCon.typeParams.head))
        inside(result.log.typeStructures.structure(aCon)) { case Types.Func(Seq(boxParam), parameter, result) =>
          assert((parameter, result) === ((Types.Param(boxParam), Types.Param(boxParam))))
        }
    }
  }

  // namespaces: their types are registered with the types of the scope

  private def color(members: InBlockDecl*) = NamespaceDef(false, NameWithPos("Color"), members)

  test("a type def can refer to a namespace's types declared below it") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "AnyColor", Ast.NamedType(Seq(NameWithPos("Color")), NameWithPos("Red"), Nil)),
      color(TypeDef(TypeDefKind.Singleton, "Red")),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(anyCon: TypeCons.WrapperTypeCon), Terms.TypeDef(redCon: TypeCons.SingletonTypeCon))) =>
        assert(result.log.typeStructures.structure(anyCon) === Types.Singleton(redCon))
    }
  }

  // companions: `::extend X` is the namespace of the type X defined in the same scope

  private def extend(name: String, members: InBlockDecl*) = NamespaceDef(true, NameWithPos(name), members)
  private def qualifiedType(namespace: String, name: String) = Ast.NamedType(Seq(NameWithPos(namespace)), NameWithPos(name), Nil)

  test("a type can refer to its companion's types, before or after the ::extend") {
    val block = Block(Seq(
      extend("Color", TypeDef(TypeDefKind.Singleton, "Red")),
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Red")),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    inside(result.value.value) {
      case (_, Seq(Terms.TypeDef(colorCon: TypeCons.WrapperTypeCon), Terms.TypeDef(redCon: TypeCons.SingletonTypeCon))) =>
        assert(redCon.definedInNs === Seq("Color"))
        assert(result.log.typeStructures.structure(colorCon) === Types.Singleton(redCon))
    }
  }

  test("::extend needs a type of the same name in the same scope, and only one ::extend") {
    val block = Block(Seq(
      extend("Missing"),
      TypeDef(TypeDefKind.Tagged, "Id", UnitType()),
      extend("Id"),
      extend("Id"),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Cannot extend `Missing`, there's no type `Missing` defined in this scope",
      "`Id` is already extended in this scope",
    ))
  }

  test("an ::extend without its type doesn't extend a namespace of the same name") {
    val block = Block(Seq(
      NamespaceDef(false, NameWithPos("Space"), Seq(TypeDef(TypeDefKind.Alias, "Inner", UnitType()))),
      extend("Space", TypeDef(TypeDefKind.Alias, "Inner", TupleType(Seq(UnitType(), UnitType())))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Cannot extend `Space`, there's no type `Space` defined in this scope"))
    // the namespace's type isn't given the structure of the ::extend's type of the same name
    assert(result.log.typeStructures.structures.values.toSeq === Seq(Types.Uni))
  }

  test("extending a singleton isn't supported yet") {
    val block = Block(Seq(TypeDef(TypeDefKind.Singleton, "Foo"), extend("Foo")))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Extending a singleton isn't supported yet"))
  }

  test("the body of a companion that failed to be registered is still checked") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Singleton, "Foo"),
      extend("Foo", TypeDef(TypeDefKind.Alias, "Inner", UnitType()), TypeDef(TypeDefKind.Alias, "Inner", UnitType())),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq(
      "Extending a singleton isn't supported yet",
      "Cannot shadow existing type with same name `Inner`",
    ))
  }

  test("a companion's type can refer to the type itself when that isn't recursive") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Red")),
      extend("Color", TypeDef(TypeDefKind.Singleton, "Red"), TypeDef(TypeDefKind.Alias, "Again", NamedType("Color"))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.isRight)
  }

  test("a type and its companion's type referring to each other is a recursive type") {
    val block = Block(Seq(
      TypeDef(TypeDefKind.Alias, "Color", qualifiedType("Color", "Again")),
      extend("Color", TypeDef(TypeDefKind.Alias, "Again", NamedType("Color"))),
    ))

    val result = ScopeAnalyzer.registerNamesAndTypes(Env.empty, block.bcs).run

    assert(result.value.left.value.map(_.message) === Seq("Recursive types aren't supported yet: `Color` -> `Again` -> `Color`"))
  }
}
