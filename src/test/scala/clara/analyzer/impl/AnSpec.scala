package clara.analyzer.impl

import clara.util.GeneralMessage
import clara.util.Message
import clara.util.Safe.SafeStringContext
import clara.asg.Uniq
import clara.ast.{NoPos, SourceInfo, SourcePos}

import clara.testutil.BaseSpec

class AnSpec extends BaseSpec {

  val messageA = GeneralMessage("A")
  val messageB = GeneralMessage("B")
  val messageC = GeneralMessage("C")
  val messageD = GeneralMessage("D")

  def anLogWarn(ms: Message*): AnLog = AnLog(warnings = ms.toVector)

  test("AnLog.append combines warnings and usage traces") {
    val valueUniq = Uniq()
    val typeUniq = Uniq()
    val sourceInfo = SourceInfo.fromString("test", "ab")
    val posA = SourcePos(sourceInfo, 0, Some(1))
    val posB = SourcePos(sourceInfo, 1, Some(2))
    val left = AnLog(Vector(messageA), UsageTrace(Map(valueUniq -> Set(posA)), Map(typeUniq -> Set(posA))))
    val right = AnLog(Vector(messageB), UsageTrace(Map(valueUniq -> Set(posB)), Map(typeUniq -> Set(posB))))

    assert(left.append(right) === AnLog(
      Vector(messageA, messageB),
      UsageTrace(Map(valueUniq -> Set(posA, posB)), Map(typeUniq -> Set(posA, posB))),
    ))
  }

  test("tellWarnings keeps the usage trace") {
    val uniq = Uniq()
    val usageTrace = UsageTrace(Map(uniq -> Set(NoPos)), Map.empty)
    val an = An.Success(1, AnLog(Vector(messageA), usageTrace)).tellWarnings(Vector(messageB))

    assert(an.log === AnLog(Vector(messageA, messageB), usageTrace))
  }

  test("tellUsage keeps the warnings") {
    val uniq = Uniq()
    val usageTrace = UsageTrace(Map(uniq -> Set(NoPos)), Map.empty)
    val an = An.Success(1, AnLog(Vector(messageA), usageTrace)).tellUsage(usageTrace)

    assert(an.log === AnLog(Vector(messageA), usageTrace))
  }

  test("withLog exposes the current log without changing it") {
    val original = An.Success(1, anLogWarn(messageA))
    val withLog = original.zipLog

    assert(withLog.value === Right((1, original.log)))
    assert(withLog.log === original.log)
  }

  test("Success.flatMap(Success)") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an = an1.flatMap(v => An.Success(v + 2, anLogWarn(messageB)))

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Right(3))
  }

  test("Success.flatMap(Failure)") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an = an1.flatMap((_: Int) => An.Failure(Vector(messageC), anLogWarn(messageB)))

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.flatMap(Success)") {
    val an1: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageA))
    val an = an1.flatMap(v => An.Success(v + 2, anLogWarn(messageB)))

    assert(an.log === anLogWarn(messageA))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.map") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an = an1.map(v => v + 2)

    assert(an.log === anLogWarn(messageA))
    assert(an.value === Right(3))
  }

  test("Failure.map") {
    val an1: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageA))
    val an = an1.map(v => v + 2)

    assert(an.log === anLogWarn(messageA))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.tellWarning") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an = an1.tellWarning(messageB)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Right(1))
  }

  test("Failure.tellWarning") {
    val an1: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageA))
    val an = an1.tellWarning(messageB)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.zip(Success)") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an2: An[Int] = An.Success(2, anLogWarn(messageB))
    val an = an1.zip(an2)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Right((1, 2)))
  }

  test("Success.zip(Failure)") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an2: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageB))
    val an = an1.zip(an2)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.zip(Success)") {
    val an1: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageA))
    val an2: An[Int] = An.Success(2, anLogWarn(messageB))
    val an = an1.zip(an2)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.zip(Failure)") {
    val an1: An[Int] = An.Failure(Vector(messageC), anLogWarn(messageA))
    val an2: An[Int] = An.Failure(Vector(messageD), anLogWarn(messageB))
    val an = an1.zip(an2)

    assert(an.log === anLogWarn(messageA, messageB))
    assert(an.value === Left(Vector(messageC, messageD)))
  }

  test("An.seq Success") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an2: An[Int] = An.Success(2, anLogWarn(messageB))
    val an3: An[Int] = An.Success(3, anLogWarn(messageC))
    val an = An.seq(Seq(an1, an2, an3))

    assert(an.log === anLogWarn(messageA, messageB, messageC))
    assert(an.value === Right(Vector(1, 2, 3)))
  }

  test("An.seq Failure") {
    val an1: An[Int] = An.Success(1, anLogWarn(messageA))
    val an2: An[Int] = An.Success(2, anLogWarn(messageB))
    val an3: An[Int] = An.Failure(Vector(messageD), anLogWarn(messageC))
    val an = An.seq(Seq(an1, an2, an3))

    assert(an.log === anLogWarn(messageA, messageB, messageC))
    assert(an.value === Left(Vector(messageD)))
  }

  test("An.step Success") {
    val items = Seq(1, 2, 3)

    val an = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      An.Success(nextResult, anLogWarn(GeneralMessage(safe"sum: ${nextResult.toString()}")))
    })

    assert(an.log === anLogWarn(GeneralMessage("sum: 1"), GeneralMessage("sum: 3"), GeneralMessage("sum: 6")))
    assert(an.value === Right(6))
  }

  test("An.step Failure") {
    val items = Seq(1, 2, 3)

    val an = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      val stepLog = anLogWarn(GeneralMessage(safe"sum: ${nextResult.toString()}"))

      if (nextResult >= 2) {
        val error = GeneralMessage(safe"failure at item: ${item.toString()}")
        An.Failure(Vector(error), stepLog)
      } else {
        An.Success(nextResult, stepLog)
      }
    })

    assert(an.log === anLogWarn(GeneralMessage("sum: 1"), GeneralMessage("sum: 3"), GeneralMessage("sum: 4")))
    assert(an.value === Left(Vector("failure at item: 2", "failure at item: 3").map(GeneralMessage(_))))
  }
}
