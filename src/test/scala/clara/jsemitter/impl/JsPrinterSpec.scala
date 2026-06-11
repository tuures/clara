package clara.jsemitter.impl

import clara.testutil.BaseSpec

import JsAst._

class JsPrinterSpec extends BaseSpec {
  def printModule(module: Module): String = JsPrinter.printModule(module)
  def printExpr(expr: Expr): String = JsPrinterImpl.printExpr(expr)
  def printContent(c: Content): String = JsPrinterImpl.printContent(c)
  def printPattern(p: Pattern): String = JsPrinterImpl.printPattern(p)

  // Expressions

  test("Undefined") {
    assert(printExpr(Undefined) === "undefined")
  }

  test("NumberLiteral") {
    assert(printExpr(NumberLiteral("42")) === "42")
  }

  test("StringLiteral: simple plain part") {
    assert(printExpr(StringLiteral("hello")) === "'hello'")
  }

  test("StringLiteral: single quoted output: single quotes are escaped but backticks are not") {
    assert(printExpr(StringLiteral("it's a backtick `")) === """'it\'s a backtick `'""")
  }

  test("StringLiteral: backticks quoted output: backticks are ecaped but single quotes are not") {
    assert(printExpr(StringLiteral(Seq(
      StringPlainPart("""it's ta backtick `"""),
      StringExpressionPart(Named("x")),
    ))) === """`it's ta backtick \`${x}`""")
  }

  test("StringLiteral: possible escapes render correctly in single quoted output") {
    assert(printExpr(StringLiteral(Seq(StringEscapePart(Seq("n", "r", "t"))))) === """'\n\r\t'""")
    assert(printExpr(StringLiteral(Seq(StringEscapePart(Seq("$"))))) === "'$'")
    assert(printExpr(StringLiteral(Seq(StringEscapePart(Seq("\"", """\"""))))) === """'"\\'""")
    assert(printExpr(StringLiteral(Seq(StringEscapePart(Seq("u0041", "u10FFFF"))))) === """'\""" + """u{0041}\""" + """u{10FFFF}'""")
  }

  test("StringLiteral: possible escapes render correctly in backtick quoted output") {
    val exp = StringExpressionPart(Named("x"))
    assert(printExpr(StringLiteral(Seq(exp, StringEscapePart(Seq("n", "r", "t"))))) === """`${x}\n\r\t`""")
    assert(printExpr(StringLiteral(Seq(exp, StringEscapePart(Seq("$"))))) === """`${x}\$`""")
    assert(printExpr(StringLiteral(Seq(exp, StringEscapePart(Seq("\"", """\"""))))) === """`${x}"\\`""")
    assert(printExpr(StringLiteral(Seq(exp, StringEscapePart(Seq("u0041", "u10FFFF"))))) === """`${x}\""" + """u{0041}\""" + """u{10FFFF}`""")
  }

  test("StringLiteral: mixed plain and escapes produces single quoted string output") {
    assert(printExpr(StringLiteral(Seq(
      StringPlainPart("a"),
      StringEscapePart(Seq("n")),
    ))) === "'a\\n'")
  }

  test("StringLiteral: mixed plain + escapes + expressions produces backtick string output") {
    assert(printExpr(StringLiteral(Seq(
      StringPlainPart("a"),
      StringEscapePart(Seq("n")),
      StringExpressionPart(Named("x")),
      StringPlainPart("c"),
    ))) === "`a\\n${x}c`")
  }

  test("ArrayLiteral: empty") {
    assert(printExpr(ArrayLiteral(Nil)) === "[]")
  }

  test("ArrayLiteral: multiple elements") {
    assert(printExpr(ArrayLiteral(Seq(NumberLiteral("1"), NumberLiteral("2")))) === "[1, 2]")
  }

  test("ObjectLiteral: empty") {
    assert(printExpr(ObjectLiteral(Nil)) === "{}")
  }

  test("ObjectLiteral: single entry") {
    assert(printExpr(ObjectLiteral(Seq("a" -> NumberLiteral("1")))) ===
      "{\n  a: 1\n}")
  }

  test("ObjectLiteral: multiple entries") {
    assert(printExpr(ObjectLiteral(Seq("a" -> NumberLiteral("1"), "b" -> NumberLiteral("2")))) ===
      "{\n  a: 1,\n  b: 2\n}")
  }

  test("Named") {
    assert(printExpr(Named("foo")) === "foo")
  }

  test("Member") {
    assert(printExpr(Member(Named("obj"), "x")) === "obj.x")
  }

  // Arrow functions

  test("UnaryArrowFunc: UnitPattern single return uses implicit return") {
    assert(printExpr(UnaryArrowFunc(UnitPattern, Seq(Return(NumberLiteral("1"))))) === "() =>\n  1")
  }

  test("UnaryArrowFunc: UnitPattern multi-statement body") {
    assert(printExpr(UnaryArrowFunc(UnitPattern, Seq(Const(NamePattern("x"), NumberLiteral("1")), Return(Named("x"))))) ===
      "() => {\n  const x = 1\n  return x\n}")
  }

  test("UnaryArrowFunc: single return uses implicit return") {
    assert(printExpr(UnaryArrowFunc(NamePattern("x"), Seq(Return(Named("x"))))) ===
      "x =>\n  x")
  }

  test("UnaryArrowFunc: multi-statement body") {
    assert(printExpr(UnaryArrowFunc(NamePattern("x"), Seq(Const(NamePattern("y"), Named("x")), Return(Named("y"))))) ===
      "x => {\n  const y = x\n  return y\n}")
  }

  test("UnaryArrowFunc: single expr body prints without block") {
    assert(printExpr(UnaryArrowFunc(NamePattern("x"), Seq(Named("x")))) ===
      "x =>\n  x")
  }

  test("UnaryArrowFunc: ArrayPattern param wraps in parens") {
    assert(printExpr(UnaryArrowFunc(ArrayPattern(Seq(NamePattern("a"), NamePattern("b"))), Seq(Return(Named("a"))))) ===
      "([a, b]) =>\n  a")
  }

  test("UnaryArrowFunc: Return(Iife(...)) unwraps the iife") {
    assert(printExpr(UnaryArrowFunc(NamePattern("x"), Seq(Return(Iife(Seq(Const(NamePattern("y"), Named("x")), Return(Named("y")))))))) ===
      "x => {\n  const y = x\n  return y\n}")
  }

  test("UnaryArrowFunc: bare Iife body unwraps the iife") {
    assert(printExpr(UnaryArrowFunc(NamePattern("x"), Seq(Iife(Seq(Return(Named("x"))))))) ===
      "x => {\n  return x\n}")
  }

  // Calls

  test("UnaryCall") {
    assert(printExpr(UnaryCall(Named("f"), NumberLiteral("1"))) === "f(1)")
  }

  test("call wraps arrow func callee in parens") {
    val func = UnaryArrowFunc(NamePattern("x"), Seq(Return(Named("x"))))
    assert(printExpr(UnaryCall(func, NumberLiteral("1"))) === "(x =>\n  x)(1)")
  }

  test("call wraps Iife callee in parens") {
    val iife = Iife(Seq(Return(NumberLiteral("1"))))
    assert(printExpr(UnaryCall(iife, NumberLiteral("2"))) === "((() =>\n  1)())(2)")
  }

  // Iife

  test("Iife") {
    assert(printExpr(Iife(Seq(Return(NumberLiteral("1"))))) === "(() =>\n  1)()")
  }

  test("Iife: multi-statement body") {
    assert(printExpr(Iife(Seq(Const(NamePattern("x"), NumberLiteral("1")), Return(Named("x"))))) ===
      "(() => {\n  const x = 1\n  return x\n})()")
  }

  // Binary operations

  test("BinaryOperation: simple") {
    assert(printExpr(BinaryOperation(Named("a"), "+", Named("b"))) === "a + b")
  }

  test("BinaryOperation: nested wraps in parens") {
    val inner = BinaryOperation(Named("a"), "+", Named("b"))
    assert(printExpr(BinaryOperation(inner, "*", Named("c"))) === "(a + b) * c")
  }

  test("BinaryOperation: nested on right side wraps in parens") {
    val inner = BinaryOperation(Named("b"), "+", Named("c"))
    assert(printExpr(BinaryOperation(Named("a"), "*", inner)) === "a * (b + c)")
  }

  // Statements

  test("Return") {
    assert(printContent(Return(Named("x"))) === "return x")
  }

  test("If: single branch, no else") {
    val stmt = IfElse(Seq(IfBranch(Named("cond"), Seq(Return(NumberLiteral("1"))))), Nil)
    assert(printContent(stmt) === "if (cond) {\n  return 1\n}")
  }

  test("If: two branches with else") {
    val stmt = IfElse(
      Seq(
        IfBranch(Named("a"), Seq(Return(NumberLiteral("1")))),
        IfBranch(Named("b"), Seq(Return(NumberLiteral("2")))),
      ),
      Seq(Return(NumberLiteral("3")))
    )
    assert(printContent(stmt) ===
      "if (a) {\n  return 1\n} else if (b) {\n  return 2\n} else {\n  return 3\n}")
  }

  // Definitions

  test("Const with NamePattern") {
    assert(printContent(Const(NamePattern("x"), NumberLiteral("42"))) === "const x = 42")
  }

  test("Const with ArrayPattern") {
    assert(printContent(Const(ArrayPattern(Seq(NamePattern("a"), NamePattern("b"))), Named("pair"))) ===
      "const [a, b] = pair")
  }

  test("Const with UnitPattern is not binding anything and just evaluates the expression") {
    assert(printContent(Const(UnitPattern, Named("x"))) === "x")
  }

  // Patterns

  test("UnitPattern") {
    assert(printPattern(UnitPattern) === "")
  }

  test("NamePattern") {
    assert(printPattern(NamePattern("x")) === "x")
  }

  test("ArrayPattern") {
    assert(printPattern(ArrayPattern(Seq(NamePattern("a"), NamePattern("b")))) === "[a, b]")
  }

  test("ArrayPattern with UnitPattern element") {
    assert(printPattern(ArrayPattern(Seq(UnitPattern, NamePattern("x")))) === "[, x]")
  }

  // Module

  test("Module: empty") {
    assert(printModule(Module(Nil)) === "")
  }

  test("Module: multiple contents separated by blank lines") {
    val module = Module(Seq(
      Const(NamePattern("x"), NumberLiteral("1")),
      Const(NamePattern("y"), NumberLiteral("2")),
    ))
    assert(printModule(module) === "const x = 1\n\nconst y = 2")
  }
}
