package clara.analyzer.impl

import clara.testutil.BaseSpec

class WriterSpec extends BaseSpec {

  test(".flatMap") {
    val w = Writer(1, Vector("A")).flatMap(v => Writer(v + 1, Vector("B")))
    assert(w.log === Vector("A", "B"))
    assert(w.value === 2)
  }

  test(".map") {
    val w = Writer(1, Vector("A")).map(v => v + 1)
    assert(w.log === Vector("A"))
    assert(w.value === 2)
  }

  test(".tell") {
    val w = Writer(1, Vector("A")).tell(Vector("B"))
    assert(w.log === Vector("A", "B"))
    assert(w.value === 1)
  }

  test(".zip") {
    val w = Writer(1, Vector("A")).zip(Writer(2, Vector("B")))
    assert(w.log === Vector("A", "B"))
    assert(w.value === (1, 2))
  }

  test("Writer.seq") {
    val w1 = Writer(1, Vector("A"))
    val w2 = Writer(2, Vector("B"))
    val w3 = Writer(3, Vector("C"))
    val w: Writer[Seq[Int], String] = Writer.seq(Seq(w1, w2, w3))

    assert(w.log === Vector("A", "B", "C"))
    assert(w.value === Seq(1, 2, 3))
  }
}
