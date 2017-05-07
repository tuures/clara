package clara

import org.scalatest._

import fastparse.all._

class HelloSpec extends FunSuite with Matchers {

  def check(input: String) = {
    Parser.parse(input) match {
      case f: Parsed.Failure =>
        //        println(f.formatExpectedAsString)
        //        println(f.formatTraces)
        throw new Exception(input + "\n" + f.extra.traced.trace)
      case s: Parsed.Success[_] =>
        //        println(parsed)
        assert(s.index == input.length)
    }
  }

  def s(input: String) = {
    test(input) { check(input) }
  }

  s("foo")
  s("() => 1")
  s("a => ()")
  s("(a, b) => a")
  s("""(a: Int, b: String) => (b, a, 1, "foobar")""")
  s("(a, b): (Int, Int) => 1")
  s("""(a: Int, (b: Int, c: Int)) => "str"""")

}
