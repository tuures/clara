package clara.analyzer

import clara.analyzer.impl.{An, AnLog}
import clara.asg.{Terms, Types}
import clara.ast.{NoPos, SourceInfo, SourceMessage, SourcePos}
import clara.testutil.BaseSpec

class AnalyzerSpec extends BaseSpec {

  def anLogWarn(ms: SourceMessage*): AnLog = AnLog(warnings = ms.toVector)

  test("AnalyzedProgram.messages includes both warnings (log) and errors") {
    val warning = SourceMessage(NoPos, "a warning")
    val error = SourceMessage(NoPos, "an error")

    val analysis = An.Invalid(Vector(error), anLogWarn(warning))
    val result = AnalyzedProgram(analysis.run)

    assert(result.program === None)
    assert(result.messages === Seq(warning, error))
  }

  test("AnalyzedProgram.messages are in source order within each source, warnings and errors together") {
    val sourceA = SourceInfo.fromString("a.clara", "0123456789")
    val sourceB = SourceInfo.fromString("b.clara", "0123456789")
    def message(source: SourceInfo, index: Int) = SourceMessage(SourcePos(source, index, None), "message")

    val warnings = Seq(message(sourceB, 1), message(sourceA, 5))
    val errors = Seq(message(sourceA, 7), message(sourceA, 3))

    val analysis = An.Invalid(errors.toVector, anLogWarn(warnings: _*))
    val result = AnalyzedProgram(analysis.run)

    assert(result.messages === Seq(message(sourceA, 3), message(sourceA, 5), message(sourceA, 7), message(sourceB, 1)))
  }

  test("AnalyzedProgram.messages includes warnings on success") {
    val warning = SourceMessage(NoPos, "a warning")

    val term = Terms.Block(Seq.empty, Types.Uni)

    val analysis = An.Valid(term, anLogWarn(warning))
    val result = AnalyzedProgram(analysis.run)

    assert(result.program === Some(term))
    assert(result.messages === Seq(warning))
  }
}
