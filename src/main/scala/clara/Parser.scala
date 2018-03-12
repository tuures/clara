package clara

object Parser {

  def parse(input: String)/*:fastparse.core.Parsed*/ = Impl.program.parse(input)

  object Impl {
    import fastparse.noApi._

    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace(" ".rep)
    }
    import White._

    import Ast._

    //////
    // Basics

    val nlPred = (_: Char) == '\n'
    val nl = P(CharPred(nlPred)).opaque("newline")

    val comma = P(CharPred(_ == ',')).opaque("comma")

    def commaSeparatedRep[T](min: Int, p: => Parser[T]) = nl.rep ~ p.rep(min, sep=(comma ~ nl.rep)) ~ comma.? ~ nl.rep

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
      val boundary = CharPred(_ == q).opaque("quote")
      val value = CharsWhile(_ != q)
      P((boundary ~ value.! ~ boundary).map(StringLiteral))
    }

    //////
    // Tuples

    def tupleSyntax[T](p: => Parser[T]) = P("(" ~ commaSeparatedRep(2, p) ~ ")")

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

    val semi = P(CharPred(_ == ';')).opaque("semicolon")

    val blockContents: Parser[Seq[BlockContent]] = {
      val sep = (nl | semi)
      P(sep.rep ~ (comment | freeDecl | valueExpr).rep(1, sep=sep.rep(1)) ~ sep.rep)
    }

    val block: Parser[Block] = P("(" ~ blockContents ~ ")").map(Block)

    //////
    // Names

    val name = P(CharsWhile(c => Character.isLetter(c)).!).opaque("name") // NOTE: Character.isLetter works only with BMP characters

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
    // Function syntax

    def funcSyntax[T1, T2](p1: => Parser[T1], p2: => Parser[T2]) = P(p1 ~ "=>" ~ nl.rep ~ p2)

    val lambda: Parser[Lambda] = funcSyntax(pattern, valueExpr).map(Lambda.tupled)

    val funcType: Parser[FuncType] = funcSyntax(simpleType, typeExpr).map(FuncType.tupled)

    //////
    // Member selection / call

    val memberOrCall: Parser[ValueExpr] = {
      def makeNode(e: ValueExpr, m: Either[String, ValueExpr]) = m match {
        case Left(name) => MemberSelection(e, name)
        case Right(argument) => Call(e, argument)
      }

      P(simple ~ (("." ~ nl.rep ~ name).map(Left(_)) | simple.map(Right(_))).rep(1)).map { case (base, parts) =>
        parts.tail.foldLeft(makeNode(base, parts.head))(makeNode)
      }
    }

    //////
    // Declarations

    val valueDef: Parser[ValueDef] = P(pattern ~ !"=>" ~ "=" ~/ nl.rep ~ valueExpr).map(ValueDef.tupled)

    val methodDef: Parser[MethodDef] = P(name ~ typed.? ~ pattern.? ~ "=" ~ valueExpr).map(MethodDef.tupled)

    val abstractMember: Parser[AbstractMember] = P(name ~ typed).map(AbstractMember.tupled)

    val memberDecl: Parser[MemberDecl] = P(valueDef | methodDef | abstractMember)

    val classBody: Parser[Seq[MemberDecl]] = {
      val sep = (nl | comma)
      P("{" ~ sep.rep ~ memberDecl.rep(0, sep=sep.rep(1)) ~ sep.rep ~ "}")
    }

    val typeParams: Parser[Seq[String]] = P("[" ~ commaSeparatedRep(1, name) ~ "]")

    val classDef: Parser[ClassDef] = P("::class" ~/ name ~ (typeParams.?.map(_.getOrElse(List()))) ~ (":" ~ name).? ~ classBody).map(ClassDef.tupled)

    val freeDecl: Parser[FreeDecl] = P(classDef | valueDef)

    val classNew: Parser[ClassNew] = P("::new" ~/ name ~ classBody).map(ClassNew.tupled)

    //////
    // Comments
    // TODO: allow start anywhere not just after newline
    val comment: Parser[Comment] = P("//" ~ CharsWhile(!nlPred(_)).!).map(Comment)

    //////
    // Top level rules

    val valueExpr: Parser[ValueExpr] = P(classNew | memberOrCall | lambda | valueAs | simple)

    val typeExpr: Parser[TypeExpr] = P(funcType | simpleType)

    val pattern: Parser[Pattern] = P(((unitPattern | tuplePattern | patternParens | namePattern) ~ typed.?).map { case (p, t) =>
      t map (PatternAs(p, _)) getOrElse p
    })

    //////
    // Start here

    val program = P(blockContents ~ End).map(Block)
  }
}
