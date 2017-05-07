package clara

object Parser {
  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  sealed trait Expression
  case class Identifier(name: String) extends Expression
  case class Unit() extends Expression
  trait Literal extends Expression
  case class IntegerLiteral(value: String) extends Literal
  case class StringLiteral(value: String) extends Literal
  case class Tuple(values: Seq[Expression]) extends Expression
  case class Lambda(parameters: Seq[Identifier], body: Expression) extends Expression
  case class Member(expression: Expression, member: Identifier) extends Expression
  case class Call(callee: Expression, arguments: Seq[Expression]) extends Expression
  case class Block(expressions: Seq[Expression]) extends Expression
  case class Assignment(identifier: Identifier, expression: Expression) extends Expression

  //////
  // Basics

  val sp = P(CharsWhile(_ == ' '))

  val nl = P(CharsWhile(_ == '\n'))

  val digit = P(CharIn('0' to '9'))

  val identifier = P(!digit ~ CharPred(c => Character.isLetter(c)).rep(1).!.map(Identifier))

  //////
  // Literals

  val integerLiteral= P(digit.rep(1).!.map(IntegerLiteral))

  val stringLiteral = {
    val q = '\''
    val boundary = CharPred(_ == q)
    val value = CharsWhile(_ != q)
    P((boundary ~ value.! ~ boundary).map(StringLiteral))
  }

  //////
  // Simple expressions

  val literal: Parser[Literal] = P(integerLiteral | stringLiteral)

  val unit: Parser[Unit] = P(P("()").map(_ => Unit()))

  val parens: Parser[Expression] = P("(" ~ expression ~ ")")

  val simple: Parser[Expression] = P(identifier | unit | parens | block | literal)

  //////
  // Tuples

  val tupleLike: Parser[Seq[Expression]] = {
    val es: Parser[Seq[Expression]] = P(expression ~ ("," ~ expression).rep(1)).map { case (first, rest) => first +: rest }
    P("(" ~ es ~ ")")
  }

  val tuple = tupleLike.map(Tuple)

  //////
  // Lambda, Member, Call

  val parameters = {
    val ids: Parser[Seq[Identifier]] = P(identifier ~ ("," ~ identifier).rep).map { case (first, rest) => first +: rest }
    P("(" ~ ids.? ~ ")")
  }

  val lambda: Parser[Lambda] = P(parameters ~ "=>" ~ simple).map(e => (e._1.getOrElse(Nil), e._2)).map(Lambda.tupled)

  val arguments: Parser[Seq[Expression]] = P(unit | parens | tupleLike).map {
    case Unit() => Nil
    case e: Expression => Seq(e)
    case e: Seq[Expression] => e
  }

  val memberOrCall: Parser[Expression] = {
    def makeNode(e: Expression, m: Either[Identifier, Seq[Expression]]) = m match {
      case Left(identifier) => Member(e, identifier)
      case Right(arguments) => Call(e, arguments)
    }

    P(simple ~ (("." ~ identifier).map(Left(_)) | arguments.map(Right(_))).rep(1)).map { case (base, parts) =>
      parts.tail.foldLeft(makeNode(base, parts.head))(makeNode)
    }
  }

  val infixUnaryCall = {
    def makeNode(e: Expression, part: (Identifier, Expression)) = {
      val (member, argument) = part

      Call(Member(e, member), Seq(argument))
    }

    P(simple ~ (sp ~ identifier ~ sp ~ simple).rep(1)).map { case (base, parts) =>
      parts.tail.foldLeft(makeNode(base, parts.head))(makeNode)
    }
  }

  //////
  // Block, Assignment

  val blockContent = P(expression.rep(sep=nl))

  val block: Parser[Block] = P("{" ~ blockContent ~ "}").map(Block)

  val assignment: Parser[Assignment] = P(identifier ~ "=" ~ expression).map(Assignment.tupled)

  //////
  // Top level rules

  val expression: Parser[Expression] = P(assignment | lambda | infixUnaryCall | memberOrCall | simple | tuple)

  val program = P(blockContent ~ End).map(Block)
}

object Clara {
  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import sext._

    println(Parser.program.parse(input).treeString)
  }
}
