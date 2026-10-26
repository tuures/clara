package clara.analyzer.impl

import clara.util.GeneralMessage
import clara.util.Message
import clara.util.Safe.SafeStringContext
import clara.asg.DefId
import clara.ast.{NoPos, SourceInfo, SourcePos}

import clara.testutil.BaseSpec

class AnSpec extends BaseSpec {

  val messageA = GeneralMessage("A")
  val messageB = GeneralMessage("B")
  val messageC = GeneralMessage("C")
  val messageD = GeneralMessage("D")

  def anLogWarn(ms: Message*): AnLog = AnLog(warnings = ms.toVector)

  test("AnLog.append combines warnings and usage traces") {
    val defIdA = DefId()
    val defIdB = DefId()
    val sourceInfo = SourceInfo.fromString("test", "ab")
    val posA = SourcePos(sourceInfo, 0, Some(1))
    val posB = SourcePos(sourceInfo, 1, Some(2))
    val left = AnLog(warnings = Vector(messageA), usageTrace = UsageTrace(Map(defIdA -> Set(posA), defIdB -> Set(posA))))
    val right = AnLog(warnings = Vector(messageB), usageTrace = UsageTrace(Map(defIdA -> Set(posB))))

    assert(left.append(right) === AnLog(
      warnings = Vector(messageA, messageB),
      usageTrace = UsageTrace(Map(defIdA -> Set(posA, posB), defIdB -> Set(posA))),
    ))
  }

  test("tellWarnings keeps the usage trace") {
    val defId = DefId()
    val usageTrace = UsageTrace(Map(defId -> Set(NoPos)))
    val result = An.Valid(1, AnLog(warnings = Vector(messageA), usageTrace = usageTrace)).tellWarnings(Vector(messageB)).run

    assert(result.log === AnLog(warnings = Vector(messageA, messageB), usageTrace = usageTrace))
  }

  test("tellUsage keeps the warnings and adds the use") {
    val defId = DefId()
    val result = An.Valid(1, anLogWarn(messageA)).tellUsage(defId, NoPos).run

    assert(result.log === AnLog(warnings = Vector(messageA), usageTrace = UsageTrace(Map(defId -> Set(NoPos)))))
  }

  test("tellUsages adds each use, the positions of one definition together") {
    val defIdA = DefId()
    val defIdB = DefId()
    val sourceInfo = SourceInfo.fromString("test", "ab")
    val posA = SourcePos(sourceInfo, 0, Some(1))
    val posB = SourcePos(sourceInfo, 1, Some(2))
    val result = An.valid(1).tellUsages(Vector(defIdA -> posA, defIdB -> posA, defIdA -> posB)).run

    assert(result.log.usageTrace === UsageTrace(Map(defIdA -> Set(posA, posB), defIdB -> Set(posA))))
  }

  test("currentLog is the log of everything analyzed before it, unchanged") {
    val result = An.Valid(1, anLogWarn(messageA)).flatMap(_ => An.currentLog).run

    assert(result.value === Right(anLogWarn(messageA)))
    assert(result.log === anLogWarn(messageA))
  }

  test("an analysis chained after another one reads what it logged, also within zip") {
    val logged = An.Valid(1, anLogWarn(messageA))

    assert(logged.zip(An.currentLog).run.value === Right((1, anLogWarn(messageA))))
  }

  test("Valid.flatMap(Valid)") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val result = an1.flatMap(v => An.Valid(v + 2, anLogWarn(messageB))).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Right(3))
  }

  test("Valid.flatMap(Invalid)") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val result = an1.flatMap((_: Int) => An.Invalid(Vector(messageC), anLogWarn(messageB))).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Invalid.flatMap(Valid)") {
    val an1: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageA))
    val result = an1.flatMap(v => An.Valid(v + 2, anLogWarn(messageB))).run

    assert(result.log === anLogWarn(messageA))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Valid.map") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val result = an1.map(v => v + 2).run

    assert(result.log === anLogWarn(messageA))
    assert(result.value === Right(3))
  }

  test("Invalid.map") {
    val an1: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageA))
    val result = an1.map(v => v + 2).run

    assert(result.log === anLogWarn(messageA))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Valid.tellWarning") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val result = an1.tellWarning(messageB).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Right(1))
  }

  test("Invalid.tellWarning") {
    val an1: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageA))
    val result = an1.tellWarning(messageB).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Valid.zip(Valid)") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val an2: An[Int] = An.Valid(2, anLogWarn(messageB))
    val result = an1.zip(an2).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Right((1, 2)))
  }

  test("Valid.zip(Invalid)") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val an2: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageB))
    val result = an1.zip(an2).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Invalid.zip(Valid)") {
    val an1: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageA))
    val an2: An[Int] = An.Valid(2, anLogWarn(messageB))
    val result = an1.zip(an2).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Left(Vector(messageC)))
  }

  test("Invalid.zip(Invalid)") {
    val an1: An[Int] = An.Invalid(Vector(messageC), anLogWarn(messageA))
    val an2: An[Int] = An.Invalid(Vector(messageD), anLogWarn(messageB))
    val result = an1.zip(an2).run

    assert(result.log === anLogWarn(messageA, messageB))
    assert(result.value === Left(Vector(messageC, messageD)))
  }

  test("An.seq Valid") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val an2: An[Int] = An.Valid(2, anLogWarn(messageB))
    val an3: An[Int] = An.Valid(3, anLogWarn(messageC))
    val result = An.seq(Seq(an1, an2, an3)).run

    assert(result.log === anLogWarn(messageA, messageB, messageC))
    assert(result.value === Right(Vector(1, 2, 3)))
  }

  test("An.seq Invalid") {
    val an1: An[Int] = An.Valid(1, anLogWarn(messageA))
    val an2: An[Int] = An.Valid(2, anLogWarn(messageB))
    val an3: An[Int] = An.Invalid(Vector(messageD), anLogWarn(messageC))
    val result = An.seq(Seq(an1, an2, an3)).run

    assert(result.log === anLogWarn(messageA, messageB, messageC))
    assert(result.value === Left(Vector(messageD)))
  }

  test("An.seq of a long sequence doesn't overflow the stack") {
    val result = An.seq(Seq.fill(100000)(An.valid(1))).run

    assert(result.value.value.length === 100000)
  }

  test("An.step Valid") {
    val items = Seq(1, 2, 3)

    val result = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      An.Valid(nextResult, anLogWarn(GeneralMessage(safe"sum: ${nextResult.toString()}")))
    }).run

    assert(result.log === anLogWarn(GeneralMessage("sum: 1"), GeneralMessage("sum: 3"), GeneralMessage("sum: 6")))
    assert(result.value === Right(6))
  }

  test("An.step Invalid") {
    val items = Seq(1, 2, 3)

    val result = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      val stepLog = anLogWarn(GeneralMessage(safe"sum: ${nextResult.toString()}"))

      if (nextResult >= 2) {
        val error = GeneralMessage(safe"failure at item: ${item.toString()}")
        An.Invalid(Vector(error), stepLog)
      } else {
        An.Valid(nextResult, stepLog)
      }
    }).run

    assert(result.log === anLogWarn(GeneralMessage("sum: 1"), GeneralMessage("sum: 3"), GeneralMessage("sum: 4")))
    assert(result.value === Left(Vector("failure at item: 2", "failure at item: 3").map(GeneralMessage(_))))
  }

  test("An.deferErrors goes on from the fallback, the errors stay logged") {
    val an = An.deferErrors(An.error(messageA): An[Int])(0).map(_ + 1)

    val stepped = an.runFrom(AnLog())

    assert(stepped === ((Some(1), AnLog(errors = Vector(messageA)))))
  }

  test("An.stepDeferring keeps the last successful result, the errors of the elements are logged") {
    val an = An.stepDeferring(Seq(1, 2, 3))(0) { (sum, item) =>
      if (item === 2) An.error(GeneralMessage(safe"failure at item: ${item.toString()}")) else An.valid(sum + item)
    }

    val stepped = an.runFrom(AnLog())

    assert(stepped === ((Some(4), AnLog(errors = Vector(GeneralMessage("failure at item: 2"))))))
  }

  test("An.escalateErrors has no result when its analysis logged errors") {
    val an = An.escalateErrors(An.deferErrors(An.error(messageA): An[Int])(0))

    val stepped = an.runFrom(AnLog())

    assert(stepped === ((None, AnLog(errors = Vector(messageA)))))
  }

  test("An.escalateErrors keeps the result when only an analysis before it logged errors") {
    // e.g. a step inside the element of a stepDeferring after an earlier element failed
    val an = An.deferErrors(An.error(messageA): An[Int])(0).flatMap(sum => An.step(Seq(1))(sum)((sum, item) => An.valid(sum + item)))

    val stepped = an.runFrom(AnLog())

    assert(stepped === ((Some(1), AnLog(errors = Vector(messageA)))))
  }

  test("a stepDeferring nested in a step element keeps what it did, its errors are reported in order") {
    val nested = (sum: Int) => An.stepDeferring(Seq(10, 20))(sum) { (sum, item) =>
      if (item === 10) An.error(GeneralMessage("inner failure")) else An.valid(sum + item)
    }

    val result = An.step(Seq(1, 2, 3))(0) {
      case (sum, 2) => nested(sum)
      case (sum, 3) => An.error(GeneralMessage("outer failure")).map(_ => sum)
      case (sum, item) => An.valid(sum + item)
    }.run

    assert(result.value === Left(Vector(GeneralMessage("inner failure"), GeneralMessage("outer failure"))))
  }

  test("a stepDeferring nested in a step element passes its result on to the next element") {
    val result = An.step(Seq(1, 2))(0) {
      case (sum, 1) => An.stepDeferring(Seq(10, 20))(sum) { (sum, item) =>
        if (item === 10) An.error(GeneralMessage("inner failure")) else An.valid(sum + item)
      }
      case (sum, item) => An.valid(sum + item).tellWarning(GeneralMessage(safe"sum: ${(sum + item).toString()}"))
    }.run

    // the second element continued from the nested stepDeferring's result (20)
    assert(result.value === Left(Vector(GeneralMessage("inner failure"))))
    assert(result.log === anLogWarn(GeneralMessage("sum: 22")))
  }

  test("errors are reported in the order they happened, also when one comes after an analysis without a result") {
    // zip runs b after a failed, b's error happens after a's
    val b = An.stepDeferring(Seq(1))(0)((_, _) => An.error(messageB))

    val result = (An.error(messageA): An[Int]).zip(b).run

    assert(result.value === Left(Vector(messageA, messageB)))
  }

  test("An.seq has a result for each analysis or none") {
    val partial = An.seq(Seq(An.valid(1), An.error(messageA), An.valid(3))).runFrom(AnLog())

    assert(partial === ((None, AnLog(errors = Vector(messageA)))))
  }

}
