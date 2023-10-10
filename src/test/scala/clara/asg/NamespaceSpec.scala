package clara.asg

import clara.testutil.BaseSpec

class NamespaceSpec extends BaseSpec {

  val ns1 = Namespace("foo" -> 1)
  val ns2 = Namespace("foo" -> 1, "bar" -> 2)

  test("add(newName) -> Some") {
    val res = ns1.add("bar" -> 2)

    assert(res === Some(Namespace("foo" -> 1, "bar" -> 2)))
  }

  test("add(existingName) -> None") {
    val res = ns1.add("foo" -> 2)

    assert(res === None)
  }

  test("addOrModify(existingName) -> Some") {
    val ns = ns2.addOrModify("foo" -> 3)

    // TODO: order of entries is preserved (foo still first) – is this good?
    assert(ns === Namespace("foo" -> 3, "bar" -> 2))
  }

  // FIXME addOrShadow doesn't really work: you can shadow twice in the same block
  // test("addOrShadow") {
  //   val ns2 = {
  //     val res = ns1.addOrShadow("bar" -> 2, ns1)

  //     assert(res === Some(Namespace(ListMap("foo" -> 1, "bar" -> 2))))
  //     res.get
  //   }

  //   val res1 = ns2.addOrShadow()
  // }
}
