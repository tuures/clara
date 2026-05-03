package clara.analyzer

import clara.analyzer.impl.An
import clara.asg.{Terms, Types}
import clara.util.GeneralMessage
import clara.testutil.BaseSpec

class AnalyzerSpec extends BaseSpec {

  test("AnalyzedProgram.messages includes both warnings (log) and errors") {
    val warning = GeneralMessage("a warning")
    val error = GeneralMessage("an error")

    val analysis = An.Failure(Vector(error), Vector(warning))
    val result = AnalyzedProgram(analysis)

    assert(result.program === None)
    assert(result.messages === Seq(warning, error))
  }

  test("AnalyzedProgram.messages includes warnings on success") {
    val warning = GeneralMessage("a warning")

    val term = Terms.Block(Seq.empty, Types.Uni)

    val analysis = An.Success(term, Vector(warning))
    val result = AnalyzedProgram(analysis)

    assert(result.program === Some(term))
    assert(result.messages === Seq(warning))
  }
}
