package clara.analyzer

import clara.analyzer.impl.{BlockAnalyzer, Env}
import clara.ast.{Ast, LiteralValue}
import clara.asg.Types

import clara.testutil.{AstTestHelpers, BaseSpec}

class AnalyzerSpec extends BaseSpec {
  import Ast.{TypeDef => _, NamedType => _, _}
  import AstTestHelpers._

  // FIXME probably makes more sense to replace this with end-to-end smoke test
  test("simple program smoketest") {
    val blockAst = Ast.Block(Seq(
      TypeDef(Ast.TypeDefKind.Opaque, "String"),
      TypeDef(Ast.TypeDefKind.Opaque, "Int"),
      TypeDef(Ast.TypeDefKind.Opaque, "Float"),
      TypeDef(Ast.TypeDefKind.Opaque, "Array", Seq(TypeParam("E"))),
      Ast.ValueDecl("NaN", NamedType("Int")),
      Ast.ValueDef(Ast.NamePattern("fooString"), Ast.StringLiteral(Seq(LiteralValue.StringPlainPart("foo")))),
      Ast.ValueDef(Ast.PatternAs(Ast.NamePattern("nanInt"), NamedType("Int")), Ast.NamedValue("NaN")),
      UnitLiteral()
    ))

    val blockTerm = BlockAnalyzer.blockTerm(Env.empty, blockAst).value.value

    val expectedType = Types.Uni

    assert(blockTerm.bcs.length === blockAst.bcs.length)
    assert(blockTerm.typ === expectedType)
  }
}
