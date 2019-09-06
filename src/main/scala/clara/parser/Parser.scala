package clara.parser

import clara.ast.{Ast, NoPos, Pos, SourcePos, SourceInfo, SourceMessage}

case class Parser(sourceName: String, input: String) {
  val sourceInfo = SourceInfo.fromString(sourceName, input)
  val impl = Parser.Impl(Some(sourceInfo))

  def parseAsProgramBlock: Either[Seq[SourceMessage], Ast.Block] = {
    import fastparse.core.Parsed

    impl.program.parse(input) match {
      case Parsed.Success(block, index) => {
        assert(index == sourceInfo.length)

        Right(block)
      }
      case Parsed.Failure(p, index, extra) => {
        val pos = SourcePos(sourceInfo, index, None)
        val msg = s"expected $p TRACE: ${extra.traced.trace}"

        Left(Seq(SourceMessage(pos, s"Parse error: $msg")))
      }
    }
  }
}

object Parser {
  case class Impl(sourceInfo: Option[SourceInfo]) {
    import fastparse.noApi._

    val nlPred = (_: Char) == '\n'

    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._

      val lineComment = P("//" ~ CharsWhile(!nlPred(_)))

      val blockComment = {
        val (start, end) = ("/*", "*/")

        P(start ~ (!end ~ AnyChar).rep ~ end)
      }

      val comment = P(lineComment | blockComment)

      NoTrace((" " | comment).rep)
    }
    import White._

    import Ast._

    def makePos(from: Int, until: Option[Int]) =
      sourceInfo.map(SourcePos(_, from, until)).getOrElse(NoPos)

    def withPos[T](p: => P[T]): P[(T, Pos)] = (Index ~ p ~ Index).
      map { case (from, t, until) => (t, makePos(from, Some(until))) }

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
    //   val impl = safe"withPos(p).map(t => f($tupleValues, t._2))"
    //
    //   safe"def pp$typeParams$valueParams = $impl"
    // }.foreach(println)
    def pp[A](p: P[Unit])(f: Pos => A) = withPos(p).map(t => f(t._2))
    def pp[A, B](p: P[A])(f: (A, Pos) => B) = withPos(p).map(t => f(t._1, t._2))
    def pp[A, B, C](p: P[(A, B)])(f: (A, B, Pos) => C) = withPos(p).map(t => f(t._1._1, t._1._2, t._2))
    def pp[A, B, C, D](p: P[(A, B, C)])(f: (A, B, C, Pos) => D) = withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._2))
    def pp[A, B, C, D, E](p: P[(A, B, C, D)])(f: (A, B, C, D, Pos) => E) = withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._1._4, t._2))

    //////
    // Basics

    val nl = CharPred(nlPred).opaque("newline")

    val comma = ","

    def commaSeparatedRep[T](min: Int, p: => P[T]) = nl.rep ~ p.rep(min, sep=(comma ~ nl.rep)) ~ comma.? ~ nl.rep

    val dot = "."

    val underscore = "_"

    //////
    // Literals

    val unitSyntax = "()"

    val unitLiteral: P[UnitLiteral] = P(pp(unitSyntax)(UnitLiteral.apply _))

    val unitType: P[UnitType] = P(pp(unitSyntax)(UnitType.apply _))

    val unitPattern: P[UnitPattern] = P(pp(unitSyntax)(UnitPattern.apply _))

    val hash = "#"

    def withUnderscores[A](digits: => P[A]): P[String] =
      (digits.! ~~ (underscore ~ nl.rep ~ digits.!).repX).map { case (head, tail) =>
        head + tail.foldLeft("")(_ + _)
      }

    // TODO was there some faster way
    val decimalDigits = CharIn('0' to '9').repX(1)
    val decimalDigitsWithUnderscore = withUnderscores(decimalDigits)

    val integerLiteral = P {
      def withPrefixAndUnderscores[A](prefix: String, digits: => P[A]): P[String] =
        hash ~~ prefix ~~ withUnderscores(digits)

      val binaryDigits = CharIn('0' to '1').repX(1)
      val binary = withPrefixAndUnderscores("b", binaryDigits).map(IntegerLiteralBinValue.apply _)

      val decimal = decimalDigitsWithUnderscore.map(IntegerLiteralDecValue.apply _)

      val hexDigits = (decimalDigits | CharIn('A' to 'F') | CharIn('a' to 'f')).repX(1)
      val hex = withPrefixAndUnderscores("x", hexDigits).map(IntegerLiteralHexValue.apply _)

      val value: P[IntegerLiteralValue] = binary | decimal | hex

      pp(value)(IntegerLiteral.apply _)
    }

    val floatLiteral = P {
      val n = decimalDigitsWithUnderscore
      val value: P[(String, String)] = n ~~ dot ~~ n

      pp(value)(FloatLiteral.apply _)
    }

    val processedStringLiteral = P {
      implicit class CharToString(c: Char) {
        def s = c.toString
      }

      val quote = '"'
      val escapeStart = '\\'
      val exprStart = '$'

      val boundary = CharPred(_ == quote).opaque("quote")

      val plainPart: P[StringLiteralPlainPart] = {
        val nothingSpecial = (c: Char) => c != quote && c != escapeStart && c != exprStart

        P(CharsWhile(nothingSpecial).!.map(StringLiteralPlainPart(_)))
      }

      // TODO use StringIn for better performance
      val escapeBody = quote.s | escapeStart.s | exprStart.s | "n" | "t"
      val escapePart: P[StringLiteralEscapePart] =
        (escapeStart.s ~~ escapeBody.!).rep(1).map(StringLiteralEscapePart(_))

      val exprPart: P[StringLiteralExpressionPart] =
        (exprStart.s ~~ (parens | namedValue)).map(StringLiteralExpressionPart(_))

      val part: P[StringLiteralPart] = P(plainPart | escapePart | exprPart)

      pp(boundary ~~ part.rep(1) ~~ boundary)(StringLiteral.apply _)
    }

    val verbatimStringLiteral = P {
      val quote = '\''
      val boundary = CharPred(_ == quote).opaque("quote")
      val startMarker: P[Int] = P(hash.repX.!.map(_.length) ~~ boundary)
      def endMarker(hashCount: Int) = P(boundary ~~ hash.repX(min=hashCount, max=hashCount/*exactly=hashCount*/))
      val valueBetweenMarkers = startMarker.flatMap { hashCount =>
        val end = endMarker(hashCount)
        val value = (!end ~~ AnyChar).repX.!

        (value ~~ end).map(s => Seq(StringLiteralPlainPart(s)))
      }

      pp(valueBetweenMarkers)(StringLiteral.apply _)
    }

    val stringLiteral = processedStringLiteral | verbatimStringLiteral

    //////
    // Tuples
    val openParens = "("
    val closeParens = ")"

    def tupleSyntax[T](p: => P[T]) = openParens ~ commaSeparatedRep(2, p) ~ closeParens

    val tuple: P[Tuple] = P(pp(tupleSyntax(valueExpr))(Tuple.apply _))

    val tupleType: P[TupleType] = P(pp(tupleSyntax(typeExpr))(TupleType.apply _))

    val tuplePattern: P[TuplePattern] = P(pp(tupleSyntax(pattern))(TuplePattern.apply _))

    //////
    // Parens

    def parensSyntax[T](p: => P[T]) = openParens ~ nl.rep ~ p ~ nl.rep ~ closeParens

    val parens: P[ValueExpr] = P(parensSyntax(valueExpr))

    val typeParens: P[TypeExpr] = P(parensSyntax(typeExpr))

    val patternParens: P[Pattern] = P(parensSyntax(pattern))

    //////
    // Blocks

    val semi = ";"

    def blockContents(acceptSingle: Boolean): P[Seq[BlockContent]] = {
      val sep = (nl | semi)
      P(sep.rep ~ (inBlockDef | valueExpr).rep(if (acceptSingle) 1 else 2, sep=sep.rep(1)) ~ sep.rep)
    }

    val block: P[Block] = P(pp(openParens ~ blockContents(false) ~ closeParens)(Block.apply _))

    //////
    // Names

    val name = P(CharsWhile(c => Character.isLetter(c)).!) // NOTE: Character.isLetter works only with BMP characters

    val namedValue: P[NamedValue] = P(pp(name)(NamedValue.apply _))

    val namedType: P[NamedType] = P(pp(name ~ maybeTypeArgs)(NamedType.apply _))

    val namePattern: P[NamePattern] = P(pp(name)(NamePattern.apply _))

    //////
    // Simple

    val simple: P[ValueExpr] = P(unitLiteral | floatLiteral | integerLiteral | stringLiteral | tuple | block | parens | namedValue)

    val simpleType: P[TypeExpr] = P(unitType | tupleType | typeParens | namedType)

    //////
    // ValueAs

    val colon = ":"

    val typed: P[TypeExpr] = P(colon ~ typeExpr)

    val valueAs: P[ValueAs] = P(pp(simple ~ typed)(ValueAs.apply _))

    //////
    // Function syntax

    val funcArrow = "=>"

    def funcSyntax[T1, T2](p1: => P[T1], p2: => P[T2]) = P(p1 ~ funcArrow ~ nl.rep ~ p2)

    val lambda: P[Lambda] = P(pp(funcSyntax(pattern, valueExpr))(Lambda.apply _))

    val funcType: P[FuncType] = P(pp(funcSyntax(simpleType, typeExpr))(FuncType.apply _))

    //////
    // Member selection / call

    val memberOrCall: P[ValueExpr] = P {
      type MemberPart = (Int, String, Seq[TypeExpr])
      type CallPart = ValueExpr
      type Part = Either[MemberPart, CallPart]

      def makeNode(startIndex: Int)(e: ValueExpr, partWithEndIndex: (Part, Int)): ValueExpr = {
        val (part, endIndex) = partWithEndIndex
        def posFrom(fromIndex: Int) = makePos(fromIndex, Some(endIndex))
        val pos = posFrom(startIndex)

        part match {
          case Left((dotIndex, name, typeArgs)) =>
            MemberSelection(e, NamedMember(name, typeArgs, posFrom(dotIndex)), pos)
          case Right(argument) =>
            Call(e, argument, pos)
        }
      }

      val member = (Index ~ dot ~ nl.rep ~ name ~ maybeTypeArgs).map(Left(_))
      val call = simple.map(Right(_))

      (Index ~ simple ~ ((member | call) ~ Index).rep(1)).map { case (startIndex, base, parts) =>
        val mk = makeNode(startIndex) _
        parts.tail.foldLeft(mk(base, parts.head))(mk)
      }
    }

    //////
    // Type parameters
    val plus = P("+")
    val minus = P("-")

    def typeListSyntax[T](p: => P[T]): P[Seq[T]] = P(bracketOpen ~ commaSeparatedRep(1, p) ~ bracketClose)

    val typeParam: P[TypeParam] = P {
      val plusOrMinusVariance: P[Variance] = P(plus.map(_ => Covariant) | minus.map(_ => Contravariant))
      val variance = P(plusOrMinusVariance.?.map(_.getOrElse(Invariant)))

      val arity: P[Int] = typeListSyntax(underscore).?.map(_.map(_.length).getOrElse(0))

      pp(variance ~ name ~ arity)(TypeParam.apply _)
    }

    val bracketOpen = "["
    val bracketClose = "]"

    val maybeTypeParams: P[Seq[TypeParam]] = typeListSyntax(typeParam).?.map(_.getOrElse(Nil))

    val maybeTypeArgs: P[Seq[TypeExpr]] = typeListSyntax(typeExpr).?.map(_.getOrElse(Nil))

    //////
    // In-block defs
    val equalsSign = "="
    val doubleColon = "::"
    def keyword(word: String) = doubleColon ~~ word

    val valueNamesDef: P[ValueNamesDef] = P(pp(pattern ~ equalsSign ~/ nl.rep ~ valueExpr)(ValueNamesDef.apply _))

    val typeDef: P[TypeDef] = P(pp(keyword("type") ~ name ~ equalsSign ~ underscore)(TypeDef.apply _))

    val inBlockDef: P[InBlockDef] = P(valueNamesDef | typeDef)

    //
    // val valueDecl: P[ValueDecl] = P(pp(name ~ typed)(ValueDecl.apply _))
    //
    // val methodDecl: P[MethodDecl] = P(pp("::method" ~ name ~ maybeTypeParams ~ typed)(MethodDecl.apply _))
    //
    // val methodDef: P[MethodDef] = P(pp("::method" ~ name ~ maybeTypeParams ~ equalsSign ~/ valueExpr)(MethodDef.apply _))
    //
    // val memberDecl: P[MemberDecl] = P(valueDef | methodDef | valueDecl | methodDecl)
    //
    // val classBody: P[Seq[MemberDecl]] = {
    //   val sep = (nl | comma)
    //   P("{" ~ sep.rep ~ memberDecl.rep(0, sep=sep.rep(1)) ~ sep.rep ~ "}")
    // }
    //
    // val classDef: P[ClassDef] = P(pp("::class" ~ name ~ maybeTypeParams ~ ("<<" ~ namedType).? ~ classBody)(ClassDef.apply _))

    // val classNew: P[ClassNew] = P(pp("::new" ~ namedType ~ classBody)(ClassNew.apply _))

    //////
    // Top level rules

    val valueExpr: P[ValueExpr] = P(/*classNew | */memberOrCall | lambda | valueAs | simple)

    val typeExpr: P[TypeExpr] = P(funcType | simpleType)

    val pattern: P[Pattern] = P(pp((unitPattern | tuplePattern | patternParens | namePattern) ~ typed.?)((p, t, pos) => t map (PatternAs(p, _, pos)) getOrElse p))

    //////
    // Start here

    val program = P(pp(blockContents(true) ~ End)(Block.apply _))
  }
}
