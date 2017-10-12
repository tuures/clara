package clara

object Parser {

  sealed trait Node
  sealed trait BlockContent extends Node
  sealed trait ValueExpr extends BlockContent
  sealed trait TypeExpr extends Node
  sealed trait Pattern extends Node
  case class ValueDef(target: Pattern, e: ValueExpr) extends BlockContent
  case class ClassDef(name: String, members: Map[String, TypeExpr]) extends BlockContent
  case class UnitLiteral() extends ValueExpr
  case class UnitType() extends TypeExpr
  case class UnitPattern() extends Pattern
  case class IntegerLiteral(value: String) extends ValueExpr
  case class StringLiteral(value: String) extends ValueExpr
  case class Tuple(es: Seq[ValueExpr]) extends ValueExpr
  case class TupleType(ts: Seq[TypeExpr]) extends TypeExpr
  case class TuplePattern(ps: Seq[Pattern]) extends Pattern
  case class Block(bcs: Seq[BlockContent]) extends ValueExpr
  case class NamedValue(name: String) extends ValueExpr
  case class NamedType(name: String) extends TypeExpr
  case class NamePattern(name: String) extends Pattern
  case class ValueAs(e: ValueExpr, t: TypeExpr) extends ValueExpr
  case class PatternAs(p: Pattern, t: TypeExpr) extends Pattern
  case class Lambda(parameter: Pattern, body: ValueExpr) extends ValueExpr
  case class FuncType(parameter: TypeExpr, result: TypeExpr) extends TypeExpr
  case class Member(e: ValueExpr, member: String) extends ValueExpr
  case class Call(callee: ValueExpr, argument: ValueExpr) extends ValueExpr

  def parse(input: String)/*:fastparse.core.Parsed*/ = Impl.program.parse(input)

  object Impl {
    import fastparse.noApi._

    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(" ".rep)
    }
    import White._

    //////
    // Basics

    val nl = P(CharsWhile(_ == '\n')).opaque("newline")

    val digit = P(CharIn('0' to '9')).opaque("digit")

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

    def tupleSyntax[T](p: => Parser[T]) = P("(" ~ nl.rep ~ p.rep(2, sep=("," ~ nl.rep)) ~ ",".? ~ nl.rep ~ ")")

    val tuple: Parser[Tuple] = tupleSyntax(valueExpr).map(Tuple)

    val tupleType: Parser[TupleType] = tupleSyntax(typeExpr).map(TupleType)

    val tuplePattern: Parser[TuplePattern] = tupleSyntax(pattern).map(TuplePattern)

    //////
    // Parens

    def parensSyntax[T](p: => Parser[T]) = P("(" ~ nl.rep ~ p ~ nl.rep ~ ")")

    val parens: Parser[ValueExpr] = parensSyntax(valueExpr)

    val typeParens: Parser[TypeExpr] = parensSyntax(typeExpr)

    val patternParens: Parser[Pattern] = parensSyntax(pattern)

    //////
    // Blocks

    val semi = P(CharsWhile(_ == ';')).opaque("semicolon")

    val blockContent: Parser[Seq[BlockContent]] = {
      val sep = (nl | semi)
      P(sep.rep ~ (valueDef | valueExpr).rep(1, sep=sep.rep) ~ sep.rep)
    }

    val block: Parser[Block] = P("(" ~ blockContent ~ ")").map(Block)

    //////
    // Names

    val name = P(!digit ~ CharPred(c => Character.isLetter(c)).rep(1).!)

    val namedValue: Parser[NamedValue] = P(name.map(NamedValue))

    val namedType: Parser[NamedType] = P(name.map(NamedType))

    val namePattern: Parser[NamePattern] = P(name.map(NamePattern))

    //////
    // Simple

    val simple: Parser[ValueExpr] = P(unitLiteral | integerLiteral | stringLiteral | tuple | block | parens | namedValue)

    val simpleType: Parser[TypeExpr] = P(unitType | tupleType | typeParens | namedType)

    //////
    // ValueAs

    val typed: Parser[TypeExpr] = P(":" ~ typeExpr)

    val valueAs: Parser[ValueAs] = P(simple ~ typed).map(ValueAs.tupled)

    //////
    // Lambda, Member, Call

    def funcSyntax[T1, T2](p1: => Parser[T1], p2: => Parser[T2]) = P(p1 ~ "=>" ~ nl.rep ~ p2)

    val lambda: Parser[Lambda] = funcSyntax(pattern, valueExpr).map(Lambda.tupled)

    val funcType: Parser[FuncType] = funcSyntax(simpleType, typeExpr).map(FuncType.tupled)

    val memberOrCall: Parser[ValueExpr] = {
      def makeNode(e: ValueExpr, m: Either[String, ValueExpr]) = m match {
        case Left(name) => Member(e, name)
        case Right(argument) => Call(e, argument)
      }

      P(simple ~ (("." ~ nl.rep ~ name).map(Left(_)) | simple.map(Right(_))).rep(1)).map { case (base, parts) =>
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

    val valueDef: Parser[ValueDef] = P("let" ~/ pattern ~ "=" ~/ nl.rep ~ valueExpr).map(ValueDef.tupled)

    //////
    // Start here

    val program = P(blockContent ~ End).map(Block)
  }
}
