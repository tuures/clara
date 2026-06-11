package clara.jsemitter

import clara.testutil.BaseSpec

import clara.asg.{Attributes, Namespace, Terms, TypeCons, Types}
import clara.ast.{LiteralValue, NoPos}
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
    val expr = Terms.IntegerLiteral(LiteralValue.IntegerDec("42"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("42"))
  }

  test("emitValueExpr: IntegerLiteral binary") {
    val expr = Terms.IntegerLiteral(LiteralValue.IntegerBin("1010"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("0b1010"))
  }

  test("emitValueExpr: IntegerLiteral hex") {
    val expr = Terms.IntegerLiteral(LiteralValue.IntegerHex("ff"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("0xff"))
  }

  test("emitValueExpr: FloatLiteral") {
    val expr = Terms.FloatLiteral(LiteralValue.Float("3", "14"), dummyType)
    assert(emitExpr(expr) === JsAst.NumberLiteral("3.14"))
  }

  test("emitValueExpr: StringLiteral plain") {
    val expr = Terms.StringLiteral(Seq(Terms.StringPlainPart("hello")), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral("hello"))
  }

  test("emitValueExpr: StringLiteral escapes") {
    val values = Seq("n", "\"", "\\", "t", "$", "r", "u0041", "u10FFFF")
    val expr = Terms.StringLiteral(Seq(Terms.StringEscapePart(values)), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral(Seq(JsAst.StringEscapePart(values))))
  }

  test("emitValueExpr: StringLiteral with expression") {
    val expr = Terms.StringLiteral(Seq(
      Terms.StringPlainPart("hello "),
      Terms.StringExpressionPart(Terms.NamedValue("x", dummyType)),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.StringLiteral(Seq(
      JsAst.StringPlainPart("hello "),
      JsAst.StringExpressionPart(JsAst.Named("x")),
    )))
  }

  test("emitValueExpr: Tuple") {
    val expr = Terms.Tuple(Seq(
      Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType),
      Terms.IntegerLiteral(LiteralValue.IntegerDec("2"), dummyType),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.ArrayLiteral(Seq(
      JsAst.NumberLiteral("1"),
      JsAst.NumberLiteral("2"),
    )))
  }

  test("emitValueExpr: Block with single expression returns it") {
    val block = Terms.Block(Seq(
      Terms.NamedValue("a", dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Named("a"))
  }

  test("emitValueExpr: Block with multiple expressions only returns last") {
    val block = Terms.Block(Seq(
      Terms.NamedValue("a", dummyType),
      Terms.NamedValue("b", dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Iife(Seq(
      JsAst.Named("a"),
      JsAst.Return(JsAst.Named("b")),
    )))
  }

  test("emitValueExpr: Block with defs and expressions") {
    val block = Terms.Block(Seq(
      Terms.ValueDef(Terms.CapturePattern("x", dummyType), Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType)),
      Terms.NamedValue("x", dummyType),
    ), dummyType)
    val result = JsEmitter.emitValueExpr(block)
    assert(result === JsAst.Iife(Seq(
      JsAst.Const(JsAst.NamePattern("x"), JsAst.NumberLiteral("1")),
      JsAst.Return(JsAst.Named("x")),
    )))
  }

  test("emitValueExpr: NamedValue") {
    val expr = Terms.NamedValue("foo", dummyType)
    assert(emitExpr(expr) === JsAst.Named("foo"))
  }

  test("emitValueExpr: Record") {
    val expr = Terms.Record(
      Namespace("a" -> Terms.Field(Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType))),
      Types.Record("a" -> dummyType)
    )
    assert(emitExpr(expr) === JsAst.ObjectLiteral(Seq("a" -> JsAst.NumberLiteral("1"))))
  }

  test("emitValueExpr: Lambda") {
    val expr = Terms.Lambda(
      Terms.CapturePattern("x", dummyType),
      Terms.NamedValue("x", dummyType),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(
      JsAst.NamePattern("x"),
      Seq(JsAst.Return(JsAst.Named("x")))
    ))
  }

  test("emitValueExpr: Piecewise with NamedConstantPattern") {
    val trueVal = Terms.NamedValue("True", dummyType)
    val falseVal = Terms.NamedValue("False", dummyType)
    val expr = Terms.Piecewise(Seq(
      (Terms.NamedConstantPattern(trueVal), Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType)),
      (Terms.NamedConstantPattern(falseVal), Terms.IntegerLiteral(LiteralValue.IntegerDec("0"), dummyType)),
    ), dummyType)
    assert(emitExpr(expr) === JsAst.UnaryArrowFunc(JsAst.NamePattern("$value"), Seq(
      JsAst.IfElse(Seq(
          JsAst.IfBranch(JsAst.BinaryOperation(JsAst.Named("$value"), "===", JsAst.Named("True")), Seq(JsAst.Return(JsAst.NumberLiteral("1")))),
          JsAst.IfBranch(JsAst.BinaryOperation(JsAst.Named("$value"), "===", JsAst.Named("False")), Seq(JsAst.Return(JsAst.NumberLiteral("0")))),
      ), Nil)
    )))
  }

  // TODO: Piecewise with CapturePattern, TuplePattern, UnitPattern (all ???)

  test("emitValueExpr: MemberSelection on field") {
    val expr = Terms.MemberSelection(
      Terms.NamedValue("obj", dummyType),
      "x",
      Terms.SelectedField,
      dummyType
    )
    assert(emitExpr(expr) === JsAst.Member(JsAst.Named("obj"), "x"))
  }

  test("emitValueExpr: MemberSelection on method with BinaryOperator emitKind") {
    val con = TypeCons.OpaqueTypeCon("Int", Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.BinaryOperator), emitName = Some("+"))
    val expr = Terms.MemberSelection(
      Terms.NamedValue("a", dummyType),
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
    val con = TypeCons.OpaqueTypeCon("String", Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.InstanceProperty), emitName = Some("length"))
    val expr = Terms.MemberSelection(
      Terms.NamedValue("s", dummyType),
      "length",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.Member(JsAst.Named("s"), "length"))
  }

  test("emitValueExpr: MemberSelection on method with no emitKind") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, NoPos)
    val attrs = Attributes.MethodAttributes()
    val expr = Terms.MemberSelection(
      Terms.NamedValue("obj", dummyType),
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
      Terms.NamedValue("f", dummyType),
      Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType),
      dummyType
    )
    assert(emitExpr(expr) === JsAst.UnaryCall(JsAst.Named("f"), JsAst.NumberLiteral("1")))
  }

  test("emitValueExpr: Call with binary operator optimization") {
    val con = TypeCons.OpaqueTypeCon("Int", Nil, NoPos)
    val attrs = Attributes.MethodAttributes(emitKind = Some(Attributes.BinaryOperator), emitName = Some("+"))
    val callee = Terms.MemberSelection(
      Terms.NamedValue("a", dummyType),
      "plus",
      Terms.SelectedMethod(con, attrs),
      dummyType
    )
    val expr = Terms.Call(callee, Terms.NamedValue("b", dummyType), dummyType)
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
        Terms.CapturePattern("x", dummyType),
        Terms.IntegerLiteral(LiteralValue.IntegerDec("42"), dummyType)
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
        ), dummyType),
        Terms.Tuple(Seq(
          Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType),
          Terms.IntegerLiteral(LiteralValue.IntegerDec("2"), dummyType),
          Terms.UnitLiteral(),
        ), dummyType)
      )
    ), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.ArrayPattern(Seq(JsAst.NamePattern("a"), JsAst.NamePattern("b"), JsAst.UnitPattern)), JsAst.ArrayLiteral(Seq(JsAst.NumberLiteral("1"), JsAst.NumberLiteral("2"), JsAst.Undefined)))
    )))
  }

  test("emitModule: singleton type def") {
    val con = TypeCons.SingletonTypeCon("True", NoPos)
    val program = Terms.Block(Seq(Terms.TypeDef(con)), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.NamePattern("True"), JsAst.StringLiteral("True"))
    )))
  }

  test("emitModule: value decl produces nothing") {
    val program = Terms.Block(Seq(Terms.ValueDecl("x")), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq.empty))
  }

  test("emitModule: method def section") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, NoPos)
    val program = Terms.Block(Seq(
      Terms.MethodDefSection(con, Terms.CapturePattern("self", dummyType), Namespace(
        "bar" -> Terms.MethodDef(Attributes.MethodAttributes(), Terms.NamedValue("self", dummyType)),
      )),
    ), dummyType)
    val module = JsEmitter.emitModule(program)
    assert(module === JsAst.Module(Seq(
      JsAst.Const(JsAst.NamePattern("Foo$Methods"), JsAst.ObjectLiteral(Seq(
        "bar" -> JsAst.UnaryArrowFunc(JsAst.NamePattern("self"), Seq(JsAst.Return(JsAst.Named("self"))))
      )))
    )))
  }

  // emitParameters

  test("emitParameters: UnitPattern") {
    assert(JsEmitter.emitParameters(Terms.UnitPattern()) === JsAst.UnitPattern)
  }

  test("emitParameters: CapturePattern") {
    assert(JsEmitter.emitParameters(Terms.CapturePattern("x", dummyType)) === JsAst.NamePattern("x"))
  }

  test("emitParameters: TuplePattern") {
    val pat = Terms.TuplePattern(Seq(
      Terms.CapturePattern("a", dummyType),
      Terms.CapturePattern("b", dummyType),
    ), dummyType)
    assert(JsEmitter.emitParameters(pat) === JsAst.ArrayPattern(Seq(
      JsAst.NamePattern("a"),
      JsAst.NamePattern("b"),
    )))
  }

  // NameMangler

  test("NameMangler: methodsCompanionName") {
    val con = TypeCons.OpaqueTypeCon("Foo", Nil, NoPos)
    assert(NameMangler.methodsCompanionName(con) === "Foo$Methods")
  }

  // Cross-cutting: emitter + printer

  test("Emitter: lambda with block body unwraps iife") {
    val program = Terms.Block(Seq(Terms.Lambda(
      Terms.CapturePattern("x", dummyType),
      Terms.Block(Seq(
        Terms.ValueDef(Terms.CapturePattern("y", dummyType), Terms.NamedValue("x", dummyType)),
        Terms.NamedValue("y", dummyType),
      ), dummyType),
      dummyType
    )), dummyType)
    assert(modulePrinted(program) === "x => {\n  const y = x\n  return y\n}")
  }

  test("Emitter: program with def and expression") {
    val program = Terms.Block(Seq(
      Terms.ValueDef(Terms.CapturePattern("x", dummyType), Terms.IntegerLiteral(LiteralValue.IntegerDec("1"), dummyType)),
      Terms.NamedValue("x", dummyType),
    ), dummyType)
    assert(modulePrinted(program) === "const x = 1\n\nx")
  }
}
