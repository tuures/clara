package clara.asg

import clara.testutil.BaseSpec

class NamespaceSpec extends BaseSpec {

  test("add(newName) -> Some") {
    val res = Namespace("foo" -> 1).add("bar" -> 2)

    assert(res === Some(Namespace("foo" -> 1, "bar" -> 2)))
  }

  test("add(existingName) -> None") {
    val res = Namespace("foo" -> 1).add("foo" -> 2)

    assert(res === None)
  }

  test("addOrModify(existingName) should modify the existing binding and put it last in iteration order") {
    val ns = Namespace("foo" -> 1, "bar" -> 2).addOrModify("foo" -> 3)

    // equality doesn't care about order
    assert(ns === Namespace("foo" -> 3, "bar" -> 2))
    // modified binding should go last in iteration order since it was added last
    assert(ns.m.toSeq === Seq("bar" -> 2, "foo" -> 3))
  }

  test("get(name) should return the item bound to the name if it exists, otherwise None") {
    val ns = Namespace("foo" -> 1)

    assert(ns.get("foo") === Some(1))
    assert(ns.get("bar") === None)
  }

  test("entries should return all name-item pairs in the namespace") {
    val ns = Namespace("foo" -> 1, "bar" -> 2)

    assert(ns.entries === Seq("foo" -> 1, "bar" -> 2))
  }

  test("mapValues should apply the function to all items in the namespace") {
    val ns = Namespace("foo" -> 1, "bar" -> 2)
    val mapped = ns.mapValues(_ * 10)

    assert(mapped === Namespace("foo" -> 10, "bar" -> 20))
  }

}
