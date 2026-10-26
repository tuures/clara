package clara.analyzer

import clara.analyzer.impl.{An, AnLog}
import clara.asg.{Terms, Types}
import clara.util.{GeneralMessage, Message}
import clara.testutil.BaseSpec

class AnalyzerSpec extends BaseSpec {

  def anLogWarn(ms: Message*): AnLog = AnLog(warnings = ms.toVector)

  test("AnalyzedProgram.messages includes both warnings (log) and errors") {
    val warning = GeneralMessage("a warning")
    val error = GeneralMessage("an error")

    val analysis = An.Invalid(Vector(error), anLogWarn(warning))
    val result = AnalyzedProgram(analysis.run)

    assert(result.program === None)
    assert(result.messages === Seq(warning, error))
  }

  test("AnalyzedProgram.messages includes warnings on success") {
    val warning = GeneralMessage("a warning")

    val term = Terms.Block(Seq.empty, Types.Uni)

    val analysis = An.Valid(term, anLogWarn(warning))
    val result = AnalyzedProgram(analysis.run)

    assert(result.program === Some(term))
    assert(result.messages === Seq(warning))
  }
}
