package clara.analyzer.impl

import clara.util.GeneralMessage
import clara.util.Safe.SafeStringContext

import clara.testutil.BaseSpec

class AnSpec extends BaseSpec {

  val messageA = GeneralMessage("A")
  val messageB = GeneralMessage("B")
  val messageC = GeneralMessage("C")
  val messageD = GeneralMessage("D")

  test("Success.flatMap(Success)") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an = an1.flatMap(v => An.Success(v + 2, Vector(messageB)))

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Right(3))
  }

  test("Success.flatMap(Failure)") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an = an1.flatMap((_: Int) => An.Failure(Vector(messageC), Vector(messageB)))

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.flatMap(Success)") {
    val an1: An[Int] = An.Failure(Vector(messageC), Vector(messageA))
    val an = an1.flatMap(v => An.Success(v + 2, Vector(messageB)))

    assert(an.log === Vector(messageA))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.map") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an = an1.map(v => v + 2)

    assert(an.log === Vector(messageA))
    assert(an.value === Right(3))
  }

  test("Failure.map") {
    val an1: An[Int] = An.Failure(Vector(messageC), Vector(messageA))
    val an = an1.map(v => v + 2)

    assert(an.log === Vector(messageA))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.tell") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an = an1.tell(messageB)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Right(1))
  }

  test("Failure.tell") {
    val an1: An[Int] = An.Failure(Vector(messageC), Vector(messageA))
    val an = an1.tell(messageB)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Success.zip(Success)") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an2: An[Int] = An.Success(2, Vector(messageB))
    val an = an1.zip(an2)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Right((1, 2)))
  }

  test("Success.zip(Failure)") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an2: An[Int] = An.Failure(Vector(messageC), Vector(messageB))
    val an = an1.zip(an2)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.zip(Success)") {
    val an1: An[Int] = An.Failure(Vector(messageC), Vector(messageA))
    val an2: An[Int] = An.Success(2, Vector(messageB))
    val an = an1.zip(an2)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Left(Vector(messageC)))
  }

  test("Failure.zip(Failure)") {
    val an1: An[Int] = An.Failure(Vector(messageC), Vector(messageA))
    val an2: An[Int] = An.Failure(Vector(messageD), Vector(messageB))
    val an = an1.zip(an2)

    assert(an.log === Vector(messageA, messageB))
    assert(an.value === Left(Vector(messageC, messageD)))
  }

  test("An.seq Success") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an2: An[Int] = An.Success(2, Vector(messageB))
    val an3: An[Int] = An.Success(3, Vector(messageC))
    val an = An.seq(Seq(an1, an2, an3))

    assert(an.log === Vector(messageA, messageB, messageC))
    assert(an.value === Right(Vector(1, 2, 3)))
  }

  test("An.seq Failure") {
    val an1: An[Int] = An.Success(1, Vector(messageA))
    val an2: An[Int] = An.Success(2, Vector(messageB))
    val an3: An[Int] = An.Failure(Vector(messageD), Vector(messageC))
    val an = An.seq(Seq(an1, an2, an3))

    assert(an.log === Vector(messageA, messageB, messageC))
    assert(an.value === Left(Vector(messageD)))
  }

  test("An.step Success") {
    val items = Seq(1, 2, 3)

    val an = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      An.Success(nextResult, Vector(GeneralMessage(safe"sum: ${nextResult.toString()}")))
    })

    assert(an.log === Vector("sum: 1", "sum: 3", "sum: 6").map(GeneralMessage(_)))
    assert(an.value === Right(6))
  }

  test("An.step Failure") {
    val items = Seq(1, 2, 3)

    val an = An.step(items)(0)((currentResult, item) => {
      val nextResult: Int = currentResult + item

      val log = Vector(GeneralMessage(safe"sum: ${nextResult.toString()}"))

      if (nextResult >= 2) {
        val error = GeneralMessage(safe"failure at item: ${item.toString()}")
        An.Failure(Vector(error), log)
      } else {
        An.Success(nextResult, log)
      }
    })

    assert(an.log === Vector("sum: 1", "sum: 3", "sum: 4").map(GeneralMessage(_)))
    assert(an.value === Left(Vector("failure at item: 2", "failure at item: 3").map(GeneralMessage(_))))
  }
}
