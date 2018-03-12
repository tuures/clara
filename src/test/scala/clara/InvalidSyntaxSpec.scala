package clara

import org.scalatest._

import fastparse.all._

class InvalidSyntaxSpec extends FunSuite {

  import Ast._

  def check(input: String, expected: Option[Node]) = {
    Parser.parse(input) match {
      case Parsed.Success(v, _) =>
        fail("parsed: \n" + v.toString())
      case _ =>
    }
  }

  def t(input: String, expected: Option[Node] = None): Unit = nt(input)(input, expected)

  def nt(name: String)(input: String, expected: Option[Node] = None): Unit = test(name) { check(input, expected) }

  t("foo square = 1")
  t("1square = 1")

}
