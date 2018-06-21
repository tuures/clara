package clara

case class Parser(sourceName: String, input: String) {
  val sourceInfo = SourceInfo.fromString(sourceName, input)

  def parseAsProgramBlock: Either[Seq[Error], Ast.Block] = {
    import fastparse.core.Parsed

    Parser.Impl(Some(sourceInfo)).program.parse(input) match {
      case Parsed.Success(block, index) => Right(block)
      case Parsed.Failure(p, index, extra) =>
        Left(Seq(Error(SourcePos(sourceInfo, index, None), "Parse error: " + extra.traced.trace)))
    }
  }
}

object Parser {
  case class Impl(sourceInfo: Option[SourceInfo]) {
    import fastparse.noApi._

    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      // TODO have comments here
      NoTrace(" ".rep)
    }
    import White._

    import Ast._

    def pos[T](p: => P[T]): P[(T, Pos)] = (Index ~ p ~ Index).
      map { case (from, t, until) =>
        (t, sourceInfo.map(SourcePos(_, from, Some(until))).getOrElse(NoPos))
      }

    // pp is GENERATED CODE:
    // List.range(1, 5).map { pArity =>
    //   val cs = List.range(0, pArity + 1).
    //     map(i => ('A' + i).toChar.toString)
    //
    //   val typeParams = cs.mkString("[", ", ", "]")
    //
    //   val tupleArgs = cs.init.mkString(", ")
    //   val pArg =
    //     if (pArity == 1) cs.head
    //     else safe"($tupleArgs)"
    //   val valueParams = safe"(p: P[$pArg])(f: ($tupleArgs, Pos) => ${cs.last})"
    //
    //   val tupleValues = (
    //     if (pArity == 1) Seq("t._1")
    //     else List.range(1, pArity + 1).map(i => safe"t._1._$i")
    //   ).mkString(", ")
    //   val impl = safe"pos(p).map(t => f($tupleValues, t._2))"
    //
    //   safe"def pp$typeParams$valueParams = $impl"
    // }.foreach(println)
    def pp[A, B](p: P[A])(f: (A, Pos) => B) = pos(p).map(t => f(t._1, t._2))
    def pp[A, B, C](p: P[(A, B)])(f: (A, B, Pos) => C) = pos(p).map(t => f(t._1._1, t._1._2, t._2))
    def pp[A, B, C, D](p: P[(A, B, C)])(f: (A, B, C, Pos) => D) = pos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._2))
    def pp[A, B, C, D, E](p: P[(A, B, C, D)])(f: (A, B, C, D, Pos) => E) = pos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._1._4, t._2))

    //////
    // Basics

    val nlPred = (_: Char) == '\n'
    val nl = P(CharPred(nlPred)).opaque("newline")

    val comma = P(CharPred(_ == ',')).opaque("comma")

    def commaSeparatedRep[T](min: Int, p: => P[T]) = nl.rep ~ p.rep(min, sep=(comma ~ nl.rep)) ~ comma.? ~ nl.rep

    val digit = P(CharIn('0' to '9')).opaque("digit")

    //////
    // Literals

    val unitSyntax = P("()")

    val unitLiteral: P[UnitLiteral] = P(unitSyntax.map(_ => UnitLiteral()))

    val unitType: P[UnitType] = P(unitSyntax.map(_ => UnitType()))

    val unitPattern: P[UnitPattern] = P(unitSyntax.map(_ => UnitPattern()))

    val integerLiteral = P(pp(digit.rep(1).!)(IntegerLiteral.apply _))

    val stringLiteral = {
      val q = '\''
      val boundary = CharPred(_ == q).opaque("quote")
      val value = CharsWhile(_ != q)
      P(pp(boundary ~ value.! ~ boundary)(StringLiteral.apply _))
    }

    //////
    // Tuples

    def tupleSyntax[T](p: => P[T]) = ("(" ~ commaSeparatedRep(2, p) ~ ")")

    val tuple: P[Tuple] = P(pp(tupleSyntax(valueExpr))(Tuple.apply _))

    val tupleType: P[TupleType] = P(pp(tupleSyntax(typeExpr))(TupleType.apply _))

    val tuplePattern: P[TuplePattern] = P(pp(tupleSyntax(pattern))(TuplePattern.apply _))

    //////
    // Parens

    def parensSyntax[T](p: => P[T]) = P("(" ~ nl.rep ~ p ~ nl.rep ~ ")")

    val parens: P[ValueExpr] = P(parensSyntax(valueExpr))

    val typeParens: P[TypeExpr] = P(parensSyntax(typeExpr))

    val patternParens: P[Pattern] = P(parensSyntax(pattern))

    //////
    // Blocks

    val semi = P(CharPred(_ == ';')).opaque("semicolon")

    def blockContents(acceptSingle: Boolean): P[Seq[BlockContent]] = {
      val sep = (nl | semi)
      P(sep.rep ~ (comment | freeDecl | valueExpr).rep(if (acceptSingle) 1 else 2, sep=sep.rep(1)) ~ sep.rep)
    }

    val block: P[Block] = P(pp("(" ~ blockContents(false) ~ ")")(Block.apply _))

    //////
    // Names

    val name = P(CharsWhile(c => Character.isLetter(c)).!).opaque("name") // NOTE: Character.isLetter works only with BMP characters

    val namedValue: P[NamedValue] = P(pp(name)(NamedValue.apply _))

    val namedType: P[NamedType] = P(pp(name ~ maybeTypeArgs)(NamedType.apply _))

    val namePattern: P[NamePattern] = P(pp(name)(NamePattern.apply _))

    //////
    // Simple

    val simple: P[ValueExpr] = P(unitLiteral | integerLiteral | stringLiteral | tuple | block | parens | namedValue)

    val simpleType: P[TypeExpr] = P(unitType | tupleType | typeParens | namedType)

    //////
    // ValueAs

    val typed: P[TypeExpr] = P(":" ~ typeExpr)

    val valueAs: P[ValueAs] = P(pp(simple ~ typed)(ValueAs.apply _))

    //////
    // Function syntax

    def funcSyntax[T1, T2](p1: => P[T1], p2: => P[T2]) = P(p1 ~ "=>" ~ nl.rep ~ p2)

    val lambda: P[Lambda] = P(pp(funcSyntax(pattern, valueExpr))(Lambda.apply _))

    val funcType: P[FuncType] = P(pp(funcSyntax(simpleType, typeExpr))(FuncType.apply _))

    //////
    // Member selection / call

    val memberOrCall: P[ValueExpr] = {
      def makeNode(e: ValueExpr, m: Either[(String, Seq[TypeExpr]), ValueExpr]) = m match {
        case Left((name, typeArgs)) => MemberSelection(e, name, typeArgs)
        case Right(argument) => Call(e, argument)
      }

      // FIXME position
      P(simple ~ (("." ~ nl.rep ~ name ~ maybeTypeArgs).map(Left(_)) | simple.map(Right(_))).rep(1)).map { case (base, parts) =>
        parts.tail.foldLeft(makeNode(base, parts.head))(makeNode)
      }
    }

    //////
    // Type parameters

    val typeParam: P[TypeParam] = {
      val plusOrMinusVariance: P[Variance] = P(P("+").map(_ => Covariant) | P("-").map(_ => Contravariant))
      val variance = P(plusOrMinusVariance.?.map(_.getOrElse(Invariant)))

      val arity: P[Int] = typeListSyntax("_").?.map(_.map(_.length).getOrElse(0))

      P(pp(variance ~ name ~ arity)(TypeParam.apply _))
    }

    def typeListSyntax[T](p: => P[T]): P[Seq[T]] = P("[" ~ commaSeparatedRep(1, p) ~ "]")

    val maybeTypeParams: P[Seq[TypeParam]] = typeListSyntax(typeParam).?.map(_.getOrElse(Nil))

    val maybeTypeArgs: P[Seq[TypeExpr]] = typeListSyntax(typeExpr).?.map(_.getOrElse(Nil))

    //////
    // Declarations

    val valueDecl: P[ValueDecl] = P(pp(name ~ typed)(ValueDecl.apply _))

    val valueDef: P[ValueDef] = P(pp(pattern ~ !"=>" ~ "=" ~/ nl.rep ~ valueExpr)(ValueDef.apply _))

    val methodDecl: P[MethodDecl] = P(pp("::method" ~ name ~ maybeTypeParams ~ typed)(MethodDecl.apply _))

    val methodDef: P[MethodDef] = P(pp("::method" ~ name ~ maybeTypeParams ~ "=" ~/ valueExpr)(MethodDef.apply _))

    val memberDecl: P[MemberDecl] = P(valueDef | methodDef | valueDecl | methodDecl)

    val classBody: P[Seq[MemberDecl]] = {
      val sep = (nl | comma)
      P("{" ~ sep.rep ~ memberDecl.rep(0, sep=sep.rep(1)) ~ sep.rep ~ "}")
    }

    val classDef: P[ClassDef] = P(pp("::classafe" ~ name ~ maybeTypeParams ~ ("<<" ~ namedType).? ~ classBody)(ClassDef.apply _))

    val freeDecl: P[FreeDecl] = P(classDef | valueDef)

    val classNew: P[ClassNew] = P(pp("::new" ~ namedType ~ classBody)(ClassNew.apply _))

    //////
    // Comments
    // TODO: allow start anywhere not just after newline
    val comment: P[Comment] = P(pp("//" ~ CharsWhile(!nlPred(_)).!)(Comment.apply _))

    //////
    // Top level rules

    val valueExpr: P[ValueExpr] = P(classNew | memberOrCall | lambda | valueAs | simple)

    val typeExpr: P[TypeExpr] = P(funcType | simpleType)

    val pattern: P[Pattern] = P(((unitPattern | tuplePattern | patternParens | namePattern) ~ typed.?).map { case (p, t) =>
      t map (PatternAs(p, _)) getOrElse p
    })

    //////
    // Start here

    val program = P(pp(blockContents(true) ~ End)(Block.apply _))
  }
}
