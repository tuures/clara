package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.asg.{Terms, Types, TypeCons}
import clara.testutil.{AstTestHelpers, BaseSpec}

class NameResolverSpec extends BaseSpec {
  import Ast.{Lambda => _, TypeDef => _, NamedType => _, ValueDecl => _, _}
  import AstTestHelpers._

  // the env of a block after its passes, names are resolved in it
  def envOf(bcs: BlockContent*): Env = BlockAnalyzerImpl.walkBlockContents(Env.empty, bcs).run.value.value.currentEnv
  def nsPath(names: String*): Vector[NameWithPos] = names.map(NameWithPos(_)).toVector

  private def color(members: InBlockDecl*) = NamespaceDef(false, NameWithPos("Color"), members)
  private def shades(members: InBlockDecl*) = NamespaceDef(false, NameWithPos("Shades"), members)
  private def extend(name: String, members: InBlockDecl*) = NamespaceDef(true, NameWithPos(name), members)

  // qualified names: the last name is looked up in the members of the namespace its nsPath names

  test("a qualified value and type are members of their namespace") {
    val env = envOf(color(TypeDef(TypeDefKind.Singleton, "Red"), ValueDef(CapturePattern("favorite"), UnitLiteral())))

    val valueResult = NameResolver.value(env, nsPath("Color"), "favorite", NoPos).run
    val typeResult = NameResolver.typeCon(env, nsPath("Color"), "Red", NoPos).run

    assert(valueResult.value.value === Terms.NamedValue("favorite", Seq("Color"), Types.Uni))
    inside(typeResult.value.value) { case con: TypeCons.SingletonTypeCon =>
      assert((con.name, con.definedInNs) === (("Red", Seq("Color"))))
    }
  }

  test("nested namespaces are reached through their outer namespace") {
    val env = envOf(color(shades(ValueDef(CapturePattern("dark"), UnitLiteral()))))

    val result = NameResolver.value(env, nsPath("Color", "Shades"), "dark", NoPos).run

    assert(result.value.value === Terms.NamedValue("dark", Seq("Color", "Shades"), Types.Uni))
  }

  test("each namespace of a qualified name is used") {
    val env = envOf(color(shades(ValueDef(CapturePattern("dark"), UnitLiteral()))))

    val result = NameResolver.value(env, nsPath("Color", "Shades"), "dark", NoPos).run

    inside(env.local.values.get("Color")) { case Some(EnvNamespaceRef(colorNsId, colorDefId)) =>
      inside(env.namespaceMembers(colorNsId).values.get("Shades")) { case Some(EnvNamespaceRef(shadesNsId, shadesDefId)) =>
        val darkDefId = env.namespaceMembers(shadesNsId).values.get("dark").get.defId
        assert(Seq(colorDefId, shadesDefId, darkDefId).forall(result.log.usageTrace.defs.contains))
      }
    }
  }

  // names that aren't what they're used as

  test("an unknown member of a namespace") {
    val env = envOf(color())

    val result = NameResolver.value(env, nsPath("Color"), "nope", NoPos).run

    assert(result.value.left.value.map(_.message) === Seq("`nope` is not a member of namespace `Color`"))
  }

  test("a type member in the middle of a qualified type is not a namespace") {
    val env = envOf(color(TypeDef(TypeDefKind.Alias, "Shade", UnitType())))

    val result = NameResolver.typeCon(env, nsPath("Color", "Shade"), "Dark", NoPos).run

    assert(result.value.left.value.map(_.message) === Seq("`Color.Shade` is not a namespace"))
  }

  test("a namespace is not a value or a type") {
    val env = envOf(color())

    val typeResult = NameResolver.typeCon(env, Nil, "Color", NoPos).run
    val valueResult = NameResolver.value(env, Nil, "Color", NoPos).run

    assert(typeResult.value.left.value.map(_.message) === Seq("`Color` is a namespace, not a type"))
    assert(valueResult.value.left.value.map(_.message) === Seq("`Color` is a namespace, not a value"))
  }

  test("a value used before its definition is an error, also through its namespace") {
    // the names pass declares the values, the values pass defines them
    val (env, _) = ScopeAnalyzer.registerNamesAndTypes(Env.empty, Seq(
      ValueDef(CapturePattern("value"), UnitLiteral()),
      color(ValueDef(CapturePattern("favorite"), UnitLiteral())),
    )).run.value.value

    val valueResult = NameResolver.value(env, Nil, "value", NoPos).run
    val memberResult = NameResolver.value(env, nsPath("Color"), "favorite", NoPos).run

    assert(valueResult.value.left.value.map(_.message) === Seq("`value` is used before its definition"))
    assert(memberResult.value.left.value.map(_.message) === Seq("`Color.favorite` is used before its definition"))
  }

  // the head of an nsPath is looked up on the value side for values, on the type side for types

  test("an inner value shadows an outer namespace in values but not in types") {
    val outerEnv = envOf(color(TypeDef(TypeDefKind.Singleton, "Red")))
    val innerEnv = outerEnv.startNestedScope.addValue("Color", Types.Record(), NoPos).run.value.value

    val valueResult = NameResolver.value(innerEnv, Nil, "Color", NoPos).run
    val typeResult = NameResolver.typeCon(innerEnv, nsPath("Color"), "Red", NoPos).run

    assert(valueResult.value.value === Terms.NamedValue("Color", Nil, Types.Record()))
    inside(typeResult.value.value) { case con: TypeCons.SingletonTypeCon => assert(con.name === "Red") }
  }

  test("an inner type shadows an outer namespace in types but not in values") {
    val outerEnv = envOf(color(ValueDef(CapturePattern("favorite"), UnitLiteral())))
    val innerCon = TypeCons.OpaqueTypeCon("Color", Nil, Nil, NoPos)
    val innerEnv = outerEnv.startNestedScope.addTypeCon("Color", innerCon, NoPos).run.value.value

    val typeResult = NameResolver.typeCon(innerEnv, Nil, "Color", NoPos).run
    val valueResult = NameResolver.value(innerEnv, nsPath("Color"), "favorite", NoPos).run

    assert(typeResult.value.value === innerCon)
    assert(valueResult.value.value === Terms.NamedValue("favorite", Seq("Color"), Types.Uni))
  }

  // a bare namespace `X` is `X.apply`

  test("a bare tagged type is its constructor, the generated apply of its companion") {
    val withoutExtend = envOf(TypeDef(TypeDefKind.Tagged, "Id", UnitType()))
    val withExtend = envOf(extend("Id"), TypeDef(TypeDefKind.Tagged, "Id", UnitType()))

    for ((env, nsPathToId, name) <- Seq((withoutExtend, nsPath(), "Id"), (withExtend, nsPath("Id"), "apply"))) {
      inside(NameResolver.value(env, nsPathToId, name, NoPos).run.value.value) {
        case Terms.NamedValue("apply", Seq("Id"), Types.Func(_, Types.Uni, _: Types.Tagged)) =>
      }
    }
  }

  test("a bare namespace or companion is its apply") {
    val namespaceEnv = envOf(NamespaceDef(false, NameWithPos("Tools"), Seq(ValueDef(CapturePattern("apply"), UnitLiteral()))))
    val companionEnv = envOf(TypeDef(TypeDefKind.Opaque, "Token"), extend("Token", ValueDef(CapturePattern("apply"), UnitLiteral())))

    val namespaceResult = NameResolver.value(namespaceEnv, Nil, "Tools", NoPos).run
    val companionResult = NameResolver.value(companionEnv, Nil, "Token", NoPos).run

    assert(namespaceResult.value.value === Terms.NamedValue("apply", Seq("Tools"), Types.Uni))
    assert(companionResult.value.value === Terms.NamedValue("apply", Seq("Token"), Types.Uni))
  }

  test("an alias has no constructor") {
    val env = envOf(extend("Unit"), TypeDef(TypeDefKind.Alias, "Unit", UnitType()))

    val result = NameResolver.value(env, Nil, "Unit", NoPos).run

    assert(result.value.left.value.map(_.message) === Seq("`Unit` is a namespace, not a value"))
  }

  // the nsPath of an expression

  test("valueNsPath is the nsPath an expression names, None when it names a value") {
    val env = envOf(color(shades(), ValueDef(CapturePattern("favorite"), UnitLiteral())))

    val namespacePath = NameResolver.valueNsPath(env, MemberSelection(NamedValue("Color"), NamedValue("Shades")))
    val valuePath = NameResolver.valueNsPath(env, MemberSelection(NamedValue("Color"), NamedValue("favorite")))

    assert(namespacePath === Some(nsPath("Color", "Shades")))
    assert(valuePath === None)
  }
}
