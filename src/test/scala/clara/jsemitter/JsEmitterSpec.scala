package clara.jsemitter

import clara.testutil.BaseSpec

import clara.asg.{Attributes, Namespace, TermLiteral, Terms, TypeCons, Types}
import clara.ast.NoPos
import clara.ast.Ast.TypeDefKind
import clara.jsemitter.impl.{JsAst, JsPrinter}

class JsEmitterSpec extends BaseSpec {
  // TODO this feels like a hack. do we need real types in Emitter in the future?
  // if not perhaps add explicit dummy type to Terms, bit like NoPos for positions in Ast?
  val dummyType = Types.Uni

  def modulePrinted(program: Terms.Block): String = JsPrinter.printModule(JsEmitter.emitModule(program))

  def emitExpr(expr: Terms.ValueExpr): JsAst.Expr = JsEmitter.emitValueExpr(expr)

  // emitValueExpr — order follows the match in JsEmitter.emitValueExpr

  test("emitValueExpr: UnitLiteral") {
    assert(emitExpr(Terms.UnitLiteral()) === JsAst.Undefined)
  }

  test("emitValueExpr: IntegerLiteral decimal") {
    val expr = Terms.IntegerLiteral(TermLiteral.IntegerDec("42"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("42"))
  }

  test("emitValueExpr: IntegerLiteral binary") {
    val expr = Terms.IntegerLiteral(TermLiteral.IntegerBin("1010"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("0b1010"))
  }

  test("emitValueExpr: IntegerLiteral hex") {
    val expr = Terms.IntegerLiteral(TermLiteral.IntegerHex("ff"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("0xff"))
  }

  test("emitValueExpr: FloatLiteral") {
    val expr = Terms.FloatLiteral(TermLiteral.Float("3", "14"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("3.14"))
  }

  test("emitValueExpr: StringLiteral plain") {
    val expr = Terms.StringLiteral(Seq(TermLiteral.StringPlainPart("hello")), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral("hello"))
  }

  test("emitValueExpr: StringLiteral escapes") {
    val values = Seq("n", "\"", "\\", "t", "$", "r", "u0041", "u10FFFF")
    val expr = Terms.StringLiteral(Seq(TermLiteral.StringEscapePart(values)), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral(Seq(JsAst.StringEscapePart(values))))
  }

  test("emitValueExpr: StringLiteral with expression") {
    val expr = Terms.StringLiteral(Seq(
      TermLiteral.StringPlainPart("hello "),
      TermLiteral.StringValueExprPart(Terms.NamedValue("x", Nil, dummyType)),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral(Seq(
      JsAst.StringPlainPart("hello "),
      JsAst.StringExpressionPart(JsAst.Named("x")),
    )))
  }

  test("emitValueExpr: Tuple") {
    val expr = Terms.Tuple(Seq(
      Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType),
      Terms.IntegerLiteral(TermLiteral.IntegerDec("2"), dummyType),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.ArrayLiteral(Seq(
      JsAst.NumberLiteral("1"),
      JsAst.NumberLiteral("2"),
    )))
  }

  test("emitValueExpr: Block with single expression returns it") {
    val block = Terms.Block(Seq(
      Terms.NamedValue("a", Nil, dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Named("a"))
  }

  test("emitValueExpr: Block with multiple expressions only returns last") {
    val block = Terms.Block(Seq(
      Terms.NamedValue("a", Nil, dummyType),
      Terms.NamedValue("b", Nil, dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Iife(Seq(
      JsAst.Named("a"),
      JsAst.Return(JsAst.Named("b")),
    )))
  }

  test("emitValueExpr: Block with defs and expressions") {
    val block = Terms.Block(Seq(
      Terms.ValueDef(Terms.CapturePattern("x", dummyType), Nil, Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType)),
      Terms.NamedValue("x", Nil, dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Iife(Seq(
      JsAst.Const(JsAst.NamePattern("x"), JsAst.NumberLiteral("1")),
      JsAst.Return(JsAst.Named("x")),
    )))
  }

  test("emitValueExpr: NamedValue") {
    val expr = Terms.NamedValue("foo", Nil, dummyType)
    assert(emitExpr(expr) === JsAst.Named("foo"))
  }

  test("emitValueExpr: NamedValue of a namespace member is qualified with its namespaces") {
    assert(emitExpr(Terms.NamedValue("dark", Seq("Color", "Shades"), dummyType)) === JsAst.Named("Color$Shades$dark"))
    assert(emitExpr(Terms.NamedValue("class", Seq("Color"), dummyType)) === JsAst.Named("Color$class"))
  }

  test("emitValueExpr: NamedValue colliding with a JS reserved name gets a $ suffix") {
    assert(emitExpr(Terms.NamedValue("class", Nil, dummyType)) === JsAst.Named("class$"))
    assert(emitExpr(Terms.NamedValue("globalThis", Nil, dummyType)) === JsAst.Named("globalThis$"))
  }

  test("emitValueExpr: ValueAs emits just the annotated value") {
    val expr = Terms.ValueAs(Terms.IntegerLiteral(TermLiteral.IntegerDec("42"), dummyType), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("42"))
  }

  test("emitValueExpr: Record") {
    val expr = Terms.Record(
      Namespace("a" -> Terms.Field(Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType))),
      Types.Record("a" -> dummyType)
    )
    assert(emitExpr(expr) === JsAst.ObjectLiteral(Seq("a" -> JsAst.NumberLiteral("1"))))
  }

  test("emitValueExpr: Lambda") {
    val expr = Terms.Lambda(
      Terms.CapturePattern("x", dummyType),
      Terms.NamedValue("x", Nil, dummyType),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(
      JsAst.NamePattern("x"),
      Seq(JsAst.Return(JsAst.Named("x")))
    ))
  }

  test("emitValueExpr: Piecewise with NamedConstantPattern") {
    val trueVal = Terms.NamedValue("True", Nil, dummyType)
    val falseVal = Terms.NamedValue("False", Nil, dummyType)
    val expr = Terms.Piecewise(Seq(
      (Terms.NamedConstantPattern(trueVal), Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType)),
      (Terms.NamedConstantPattern(falseVal), Terms.IntegerLiteral(TermLiteral.IntegerDec("0"), dummyType)),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(JsAst.NamePattern("$value"), Seq(
      JsAst.IfElse(Seq(
          JsAst.IfBranch(JsAst.BinaryOperation(JsAst.Named("$value"), "===", JsAst.Named("True")), Seq(JsAst.Return(JsAst.NumberLiteral("1")))),
          JsAst.IfBranch(JsAst.BinaryOperation(JsAst.Named("$value"), "===", JsAst.Named("False")), Seq(JsAst.Return(JsAst.NumberLiteral("0")))),
      ), Nil)
    )))
  }

  test("emitValueExpr: Piecewise with a capture binds the matched value and needs no condition") {
    val expr = Terms.Piecewise(Seq(
      (Terms.CapturePattern("n", dummyType), Terms.NamedValue("n", Nil, dummyType)),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(JsAst.NamePattern("$value"), Seq(
      JsAst.Const(JsAst.NamePattern("n"), JsAst.Named("$value")),
      JsAst.Return(JsAst.Named("n")),
    )))
  }

  test("emitValueExpr: Piecewise with TuplePattern checks the tuple and its elements and binds captures") {
    val value = JsAst.Named("$value")
    val expr = Terms.Piecewise(Seq(
      (
        Terms.TuplePattern(Seq(Terms.UnitPattern(), Terms.CapturePattern("x", dummyType)), dummyType),
        Terms.NamedValue("x", Nil, dummyType),
      ),
      (Terms.WildcardPattern(dummyType), Terms.IntegerLiteral(TermLiteral.IntegerDec("0"), dummyType)),
    ), dummyType)
    val condition = Seq[JsAst.Expr](
      JsAst.UnaryCall(JsAst.Member(JsAst.Member(JsAst.Named("globalThis"), "Array"), "isArray"), value),
      JsAst.BinaryOperation(JsAst.Member(value, "length"), "===", JsAst.NumberLiteral("2")),
      JsAst.BinaryOperation(JsAst.Index(value, JsAst.NumberLiteral("0")), "===", JsAst.Undefined),
    ).reduceLeft(JsAst.BinaryOperation(_, "&&", _))
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(JsAst.NamePattern("$value"), Seq(
      JsAst.IfElse(Seq(
        JsAst.IfBranch(condition, Seq(
          JsAst.Const(JsAst.NamePattern("x"), JsAst.Index(value, JsAst.NumberLiteral("1"))),
          JsAst.Return(JsAst.Named("x")),
        )),
      ), Seq(JsAst.Return(JsAst.NumberLiteral("0")))),
    )))
  }

  test("emitValueExpr: MemberSelection on field") {
    val expr = Terms.MemberSelection(
      Terms.NamedValue("obj", Nil, dummyType),
      "x",
      Terms.SelectedField,
      dummyType
    )
    assert(emitExpr(expr) === JsAst.Member(JsAst.Named("obj"), "x"))
  }

  test("emitValueExpr: MemberSelection on method with BinaryOperator emitKind") {
    val con = TypeCons.OpaqueTypeCon("Int", Nil, Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.BinaryOperator), emitName = Some("+"))
    val expr = Terms.MemberSelection(
      Terms.NamedValue("a", Nil, dummyType),
      "plus",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    val result = emitExpr(expr)
    inside(result) { case JsAst.UnaryArrowFunc(JsAst.NamePattern("_"), Seq(JsAst.Return(binOp: JsAst.BinaryOperation))) =>
      assert(binOp.operator === "+")
    }
  }

  test("emitValueExpr: MemberSelection on method with InstanceProperty emitKind") {
    val con = TypeCons.OpaqueTypeCon("String", Nil, Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.InstanceProperty), emitName = Some("length"))
    val expr = Terms.MemberSelection(
      Terms.NamedValue("s", Nil, dummyType),
      "length",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.Member(JsAst.Named("s"), "length"))
  }

  test("emitValueExpr: MemberSelection on method with no emitKind") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, Nil, NoPos)
    val attrs = Attributes.MethodAttributes()
    val expr = Terms.MemberSelection(
      Terms.NamedValue("obj", Nil, dummyType),
      "bar",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.UnaryCall(
      JsAst.Member(JsAst.Named("Foo$Methods"), "bar"),
      JsAst.Named("obj")
    ))
  }

  test("emitValueExpr: Call") {
    val expr = Terms.Call(
      Terms.NamedValue("f", Nil, dummyType),
      Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.UnaryCall(JsAst.Named("f"), JsAst.NumberLiteral("1")))
  }

  test("emitValueExpr: Call with binary operator optimization") {
    val con = TypeCons.OpaqueTypeCon("Int", Nil, Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.BinaryOperator), emitName = Some("+"))
    val callee = Terms.MemberSelection(
      Terms.NamedValue("a", Nil, dummyType),
      "plus",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    val expr = Terms.Call(callee, Terms.NamedValue("b", Nil, dummyType), dummyType)
    assert(emitExpr(expr) === JsAst.BinaryOperation(JsAst.Named("a"), "+", JsAst.Named("b")))
  }

  // emitModule / emitBlockContent

  test("emitModule: empty block") {
    val program = Terms.Block(Seq.empty, dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq.empty))
  }

  test("emitModule: value def") {
    val program = Terms.Block(Seq(
      Terms.ValueDef(
        Terms.CapturePattern("x", dummyType), Nil,
        Terms.IntegerLiteral(TermLiteral.IntegerDec("42"), dummyType)
      )
    ), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.NamePattern("x"), JsAst.NumberLiteral("42"))
    )))
  }

  test("emitModule: value def with tuple pattern") {
    val program = Terms.Block(Seq(
      Terms.ValueDef(
        Terms.TuplePattern(Seq(
          Terms.CapturePattern("a", dummyType),
          Terms.CapturePattern("b", dummyType),
          Terms.UnitPattern(),
        ), dummyType), Nil,
        Terms.Tuple(Seq(
          Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType),
          Terms.IntegerLiteral(TermLiteral.IntegerDec("2"), dummyType),
          Terms.UnitLiteral(),
        ), dummyType)
      )
    ), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.ArrayPattern(Seq(JsAst.NamePattern("a"), JsAst.NamePattern("b"), JsAst.IgnoredPattern)), JsAst.ArrayLiteral(Seq(JsAst.NumberLiteral("1"), JsAst.NumberLiteral("2"), JsAst.Undefined)))
    )))
  }

  test("emitModule: singleton type def") {
    val con = TypeCons.SingletonTypeCon("True", Nil, NoPos)
    val program = Terms.Block(Seq(Terms.TypeDef(con)), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.NamePattern("True"), JsAst.ObjectLiteral(Seq("$type" -> JsAst.StringLiteral("True"))))
    )))
  }

  test("emitModule: tagged and boxed type defs emit their constructors as the apply of their companion") {
    val program = Terms.Block(Seq(
      Terms.TypeDef(TypeCons.WrapperTypeCon(TypeDefKind.Tagged, "Id", Nil, Nil, NoPos)),
      Terms.TypeDef(TypeCons.WrapperTypeCon(TypeDefKind.Boxed, "Some", Nil, Seq("Option"), NoPos)),
      Terms.TypeDef(TypeCons.WrapperTypeCon(TypeDefKind.Alias, "Unit", Nil, Nil, NoPos)),
    ), dummyType)
    assert(modulePrinted(program) ===
      "const Id$apply = $value =>\n  $value\n\nconst Option$Some$apply = $value =>\n  ({\n    $type: 'Option.Some',\n    $value: $value\n  })")
  }

  test("emitModule: namespace members are top-level consts qualified with their namespaces") {
    // type defs know their namespace from their con, value defs from definedInNs
    val program = Terms.Block(Seq(
      Terms.TypeDef(TypeCons.SingletonTypeCon("Red", Seq("Color"), NoPos)),
      Terms.ValueDef(Terms.CapturePattern("dark", dummyType), Seq("Color", "Shades"), Terms.NamedValue("Red", Seq("Color"), dummyType)),
    ), dummyType)
    assert(modulePrinted(program) === "const Color$Red = {\n  $type: 'Color.Red'\n}\n\nconst Color$Shades$dark = Color$Red")
  }

  test("emitModule: value decl produces nothing") {
    val program = Terms.Block(Seq(Terms.ValueDecl("x")), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq.empty))
  }

  test("emitModule: method def section") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, Nil, NoPos)
    val program = Terms.Block(Seq(
      Terms.MethodDefSection(con, Terms.CapturePattern("self", dummyType), Namespace(
        "bar" -> Terms.MethodDef(Attributes.MethodAttributes(), Terms.NamedValue("self", Nil, dummyType)),
      )),
    ), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.NamePattern("Foo$Methods"), JsAst.ObjectLiteral(Seq(
        "bar" -> JsAst.UnaryArrowFunc(JsAst.NamePattern("self"), Seq(JsAst.Return(JsAst.Named("self"))))
      )))
    )))
  }

  // emitBindingPattern

  test("emitBindingPattern: WildcardPattern binds nothing") {
    assert(JsEmitter.emitBindingPattern(Terms.WildcardPattern(dummyType), Nil) === JsAst.IgnoredPattern)
  }

  test("emitBindingPattern: UnitPattern") {
    assert(JsEmitter.emitBindingPattern(Terms.UnitPattern(), Nil) === JsAst.IgnoredPattern)
  }

  test("emitBindingPattern: StringPattern with a single capture binds the string") {
    val pat = Terms.StringPattern(Seq(TermLiteral.StringCapturePart(Terms.CapturePattern("s", dummyType))), dummyType)
    assert(JsEmitter.emitBindingPattern(pat, Nil) === JsAst.NamePattern("s"))
  }

  test("emitBindingPattern: TuplePattern") {
    val pat = Terms.TuplePattern(Seq(
      Terms.CapturePattern("a", dummyType),
      Terms.WildcardPattern(dummyType),
      Terms.CapturePattern("b", dummyType),
    ), dummyType)
    assert(JsEmitter.emitBindingPattern(pat, Nil) === JsAst.ArrayPattern(Seq(
      JsAst.NamePattern("a"),
      JsAst.IgnoredPattern,
      JsAst.NamePattern("b"),
    )))
  }

  test("emitBindingPattern: CapturePattern") {
    assert(JsEmitter.emitBindingPattern(Terms.CapturePattern("x", dummyType), Nil) === JsAst.NamePattern("x"))
  }

  test("emitBindingPattern: NamedConstantPattern binds nothing") {
    val pat = Terms.NamedConstantPattern(Terms.NamedValue("True", Nil, dummyType))
    assert(JsEmitter.emitBindingPattern(pat, Nil) === JsAst.IgnoredPattern)
  }

  test("emitBindingPattern: PatternAs emits its pattern") {
    val pat = Terms.PatternAs(Terms.CapturePattern("x", dummyType), dummyType)
    assert(JsEmitter.emitBindingPattern(pat, Nil) === JsAst.NamePattern("x"))
  }

  // NameMangler

  test("NameMangler: methodsCompanionName") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, Nil, NoPos)
    assert(NameMangler.methodsCompanionName(con) === "Foo$Methods")
  }

  test("NameMangler: valueName suffixes only reserved names") {
    assert(NameMangler.valueName("foo", Nil) === "foo")
    assert(NameMangler.valueName("Array", Nil) === "Array")
    assert(NameMangler.valueName("undefined", Nil) === "undefined$")
  }

  // Cross-cutting: emitter + printer

  test("Emitter: lambda with block body unwraps iife") {
    val program = Terms.Block(Seq(Terms.Lambda(
      Terms.CapturePattern("x", dummyType),
      Terms.Block(Seq(
        Terms.ValueDef(Terms.CapturePattern("y", dummyType), Nil, Terms.NamedValue("x", Nil, dummyType)),
        Terms.NamedValue("y", Nil, dummyType),
      ), dummyType),
      dummyType
    )), dummyType)
    assert(modulePrinted(program) === "x => {\n  const y = x\n  return y\n}")
  }

  test("Emitter: program with def and expression") {
    val program = Terms.Block(Seq(
      Terms.ValueDef(Terms.CapturePattern("x", dummyType), Nil, Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType)),
      Terms.NamedValue("x", Nil, dummyType),
    ), dummyType)
    assert(modulePrinted(program) === "const x = 1\n\nx")
  }

  test("Emitter: wildcards bind nothing, so several of them don't collide") {
    val pair = Terms.NamedValue("pair", Nil, dummyType)
    val program = Terms.Block(Seq(
      Terms.ValueDef(Terms.TuplePattern(Seq(Terms.WildcardPattern(dummyType), Terms.WildcardPattern(dummyType)), dummyType), Nil, pair),
      Terms.ValueDef(Terms.WildcardPattern(dummyType), Nil, pair),
      Terms.ValueDef(Terms.WildcardPattern(dummyType), Nil, pair),
      Terms.Lambda(Terms.WildcardPattern(dummyType), pair, dummyType),
    ), dummyType)
    assert(modulePrinted(program) === "const [, ] = pair\n\npair\n\npair\n\n() =>\n  pair")
  }

  test("Emitter: Clara names colliding with JS reserved names are suffixed where defined, captured and used") {
    val program = Terms.Block(Seq(
      Terms.ValueDef(Terms.CapturePattern("delete", dummyType), Nil, Terms.IntegerLiteral(TermLiteral.IntegerDec("1"), dummyType)),
      Terms.Piecewise(Seq(
        (Terms.CapturePattern("eval", dummyType), Terms.NamedValue("delete", Nil, dummyType)),
      ), dummyType),
    ), dummyType)
    assert(modulePrinted(program) === "const delete$ = 1\n\n$value => {\n  const eval$ = $value\n  return delete$\n}")
  }
}
