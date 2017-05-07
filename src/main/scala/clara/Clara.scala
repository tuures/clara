package clara

object ParserImpl {
  import fastparse.noApi._

  val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  sealed trait BlockContent
  sealed trait ValueExpr extends BlockContent
  sealed trait TypeExpr
  sealed trait Pattern
  case class ValueDef(target: Pattern, e: ValueExpr) extends BlockContent
  case class UnitLiteral() extends ValueExpr
  case class UnitType() extends TypeExpr
  case class UnitPattern() extends Pattern
  case class IntegerLiteral(value: String) extends ValueExpr
  case class StringLiteral(value: String) extends ValueExpr
  case class Block(es: Seq[BlockContent]) extends ValueExpr
  case class NamedValue(name: String) extends ValueExpr
  case class NamedType(name: String) extends TypeExpr
  case class NamePattern(name: String) extends Pattern
  case class ValueAs(e: ValueExpr, t: TypeExpr) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr) extends Pattern
  case class Tuple(es: Seq[ValueExpr]) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr]) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern]) extends Pattern
  case class Lambda(parameter: Pattern, body: ValueExpr) extends ValueExpr
  case class FuncType(parameter: TypeExpr, result: TypeExpr) extends TypeExpr
  case class Member(e: ValueExpr, member: String) extends ValueExpr
  case class Call(callee: ValueExpr, argument: ValueExpr) extends ValueExpr

  //////
  // Basics

  val nl = P(CharsWhile(_ == '\n'))

  val digit = P(CharIn('0' to '9'))

  //////
  // Literals

  val unit = P("()")

  val unitLiteral: Parser[UnitLiteral] = P(unit.map(_ => UnitLiteral()))

  val unitType: Parser[UnitType] = P(unit.map(_ => UnitType()))

  val unitPattern: Parser[UnitPattern] = P(unit.map(_ => UnitPattern()))

  val integerLiteral = P(digit.rep(1).!.map(IntegerLiteral))

  val stringLiteral = {
    val q = '\''
    val boundary = CharPred(_ == q)
    val value = CharsWhile(_ != q)
    P((boundary ~ value.! ~ boundary).map(StringLiteral))
  }

  //////
  // Tuples

  val tuple: Parser[Tuple] = P("(" ~ valueExpr.rep(2, sep=",") ~ ")").map(Tuple)

  val tupleType: Parser[TupleType] = P("(" ~ typeExpr.rep(2, sep=",") ~ ")").map(TupleType)

  val tuplePattern: Parser[TuplePattern] = P("(" ~ pattern.rep(2, sep=",") ~ ")").map(TuplePattern)

  //////
  // Parens, Block

  val parens: Parser[ValueExpr] = P("(" ~ valueExpr ~ ")")

  val typeParens: Parser[TypeExpr] = P("(" ~ typeExpr ~ ")")

  val patternParens: Parser[Pattern] = P("(" ~ pattern ~ ")")

  val semi = P(CharsWhile(_ == ';'))

  val exprSep = P(nl | semi)

  val blockContent: Parser[Seq[BlockContent]] = P(exprSep.rep ~ (valueDef | valueExpr).rep(1, sep=exprSep) ~ exprSep.rep)

  val block: Parser[Block] = P("{" ~ blockContent ~ "}").map(Block)

  //////
  // NamedValue, NamedType, NamePattern

  val name = P(!digit ~ CharPred(c => Character.isLetter(c)).rep(1).!)

  val namedValue: Parser[NamedValue] = P(name.map(NamedValue))

  val namedType: Parser[NamedType] = P(name.map(NamedType))

  val namePattern: Parser[NamePattern] = P(name.map(NamePattern))

  //////
  // Simple

  val simple: Parser[ValueExpr] = P(unitLiteral | integerLiteral | stringLiteral | tuple | parens | block | namedValue)

  val simpleType: Parser[TypeExpr] = P(unitType | tupleType | typeParens | namedType)

  //////
  // ValueAs

  val typed: Parser[TypeExpr] = P(":" ~ typeExpr)

  val valueAs: Parser[ValueAs] = P(simple ~ typed).map(ValueAs.tupled)

  //////
  // Lambda, Member, Call

  val lambda: Parser[Lambda] = P(pattern ~ "=>" ~ valueExpr).map(Lambda.tupled)

  val funcType: Parser[FuncType] = P(simpleType ~ "=>" ~ typeExpr).map(FuncType.tupled)

  val memberOrCall: Parser[ValueExpr] = {
    def makeNode(e: ValueExpr, m: Either[String, ValueExpr]) = m match {
      case Left(name) => Member(e, name)
      case Right(argument) => Call(e, argument)
    }

    P(simple ~ (("." ~ name).map(Left(_)) | simple.map(Right(_))).rep(1)).map { case (base, parts) =>
      parts.tail.foldLeft(makeNode(base, parts.head))(makeNode)
    }
  }

  //////
  // Top level rules

  val valueExpr: Parser[ValueExpr] = P(memberOrCall | lambda | valueAs | simple)

  val typeExpr: Parser[TypeExpr] = P(funcType | simpleType)

  val pattern: Parser[Pattern] = P(((unitPattern | tuplePattern | patternParens | namePattern) ~ typed.?).map { case (p, t) =>
    t map (PatternAs(p, _)) getOrElse p
  })

  val valueDef: Parser[ValueDef] = P("let" ~/ pattern ~ "=" ~/ valueExpr).map(ValueDef.tupled)

  //////
  // Start here

  val program = P(blockContent ~ End).map(Block)
}

object Parser {
  def parse(input: String) = ParserImpl.program.parse(input)
}

object Clara {
  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import sext._

    val res = Parser.parse(input)

    import fastparse.core.Parsed
    res match {
      case Parsed.Success(v, index) =>
        println(v.treeString)
      case Parsed.Failure(p, index, extra) =>
        println(extra.traced.trace)
    }
  }
}
