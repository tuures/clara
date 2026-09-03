package clara.analyzer.impl

import clara.ast.{Ast, NoPos}
import clara.testutil.{AnalyzerTestPrelude, BaseSpec}

class TypeExprAnalyzerSpec extends BaseSpec {
  test("named type constructor records the resolved constructor usage") {
    val result = TypeExprAnalyzer.namedTypeCon(
      AnalyzerTestPrelude.env,
      Ast.NameWithPos("Int", NoPos),
    )

    assert(result.value.value === AnalyzerTestPrelude.intCon)
    assert(result.log.usageTrace.typeCons === Map(AnalyzerTestPrelude.intCon.uniq -> Set(NoPos)))
  }
}
