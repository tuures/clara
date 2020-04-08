package clara.parser

import clara.ast.{Ast, LiteralValue, NoPos, Pos, SourcePos, SourceInfo, SourceMessage}

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

// TODO add more cuts to optimise and improve the error messages
// https://www.lihaoyi.com/fastparse/#Cuts
object Parser {
  case class Impl(sourceInfo: Option[SourceInfo]) {
    import fastparse.noApi._

    val nlPred = (_: Char) == '\n'

    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._

      val lineComment = P("//" ~ (End | CharPred(nlPred) | CharsWhile(!nlPred(_))))

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

    def anythingBefore[T](end: P[T]) = (!end ~~ AnyChar).repX

    val dot = "."

    val underscore = "_"

    //////
    // Literals

    val topTypeSyntax = "⊤"
    val bottomTypeSyntax = "⊥"

    val topType: P[TopType] = P(pp(topTypeSyntax)(TopType.apply _))

    val bottomType: P[BottomType] = P(pp(bottomTypeSyntax)(BottomType.apply _))

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
      val binary = withPrefixAndUnderscores("b", binaryDigits).map(LiteralValue.IntegerBin.apply _)

      val decimal = decimalDigitsWithUnderscore.map(LiteralValue.IntegerDec.apply _)

      val hexDigits = (decimalDigits | CharIn('A' to 'F') | CharIn('a' to 'f')).repX(1)
      val hex = withPrefixAndUnderscores("x", hexDigits).map(LiteralValue.IntegerHex.apply _)

      val value: P[LiteralValue.Integer] = binary | decimal | hex

      pp(value)(IntegerLiteral.apply _)
    }

    val floatLiteral = P {
      val n = decimalDigitsWithUnderscore
      val value = (n ~~ dot ~~ n).map(LiteralValue.Float.tupled)

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

      val plainPart: P[LiteralValue.StringPlainPart] = {
        val nothingSpecial = (c: Char) => c != quote && c != escapeStart && c != exprStart

        P(CharsWhile(nothingSpecial).!.map(LiteralValue.StringPlainPart(_)))
      }

      // TODO use StringIn for better performance
      val escapeBody = quote.s | escapeStart.s | exprStart.s | "n" | "t"
      val escapePart: P[LiteralValue.StringEscapePart] =
        (escapeStart.s ~~ escapeBody.!).rep(1).map(LiteralValue.StringEscapePart(_))

      val exprPart: P[LiteralValue.StringExpressionPart] =
        (exprStart.s ~~ (parens | namedValue)).map(LiteralValue.StringExpressionPart(_))

      val part: P[LiteralValue.StringPart] = P(plainPart | escapePart | exprPart)

      pp(boundary ~~ part.rep(1) ~~ boundary)(StringLiteral.apply _)
    }

    val verbatimStringLiteral = P {
      val quote = '\''
      val boundary = CharPred(_ == quote).opaque("quote")
      val startMarker: P[Int] = P(hash.repX.!.map(_.length) ~~ boundary)
      def endMarker(hashCount: Int) = P(boundary ~~ hash.repX(min=hashCount, max=hashCount/*exactly=hashCount*/))
      val valueBetweenMarkers = startMarker.flatMap { hashCount =>
        val end = endMarker(hashCount)
        // FIXME anythingBefore
        val value = (!end ~~ AnyChar).repX.!

        (value ~~ end).map(s => Seq(LiteralValue.StringPlainPart(s)))
      }

      pp(valueBetweenMarkers)(StringLiteral.apply _)
    }

    val stringLiteral = processedStringLiteral | verbatimStringLiteral

    //////
    // Tuples
    val openParens = "("
    val closeParens = ")"

    def tupleSyntax[T](element: => P[T]) = openParens ~ commaSeparatedRep(2, element) ~ closeParens

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

    val namedType: P[NamedType] = P(pp(name /*~ maybeTypeArgs*/)(NamedType.apply _))

    val namePattern: P[NamePattern] = P(pp(name)(NamePattern.apply _))

    //////
    // ValueAs

    val colon = ":"

    val typed: P[TypeExpr] = P(colon ~ typeExpr)

    val valueAs: P[ValueAs] = P(pp(NoCut(simple ~ typed))(ValueAs.apply _))

    //////
    // Record

    val braceOpen = "{"
    val braceClose = "}"
    val equalsSign = "="

    def recordSyntax[T](field: => P[T]) = {
      val sep = (nl | comma) ~ nl.rep
      braceOpen ~ nl.rep ~ field.rep(sep=sep) ~ comma.? ~ nl.rep ~ braceClose
    }

    val fieldDef = P(pp(name ~ typed.? ~ equalsSign ~ valueExpr)(FieldDef.apply _))
    val record = P(pp(recordSyntax(fieldDef))(Record.apply _))

    val fieldDecl = P(pp(name ~ typed)(FieldDecl.apply _))
    val recordType = P(pp(recordSyntax(fieldDecl))(RecordType.apply _))

    //////
    // Simple

    val simple: P[ValueExpr] = P(unitLiteral | floatLiteral | integerLiteral | stringLiteral | tuple | block | parens | namedValue | record | newExpr)

    val simpleType: P[TypeExpr] = P(topType | bottomType | unitType | tupleType | typeParens | namedType | recordType)

    //////
    // Function syntax

    val funcArrow = "=>"

    def funcSyntax[T1, T2](in: => P[T1], out: => P[T2]) = P(in ~ funcArrow ~ nl.rep ~ out)

    val lambda: P[Lambda] = P(pp(funcSyntax(pattern, valueExpr))(Lambda.apply _))

    val funcType: P[FuncType] = P(pp(funcSyntax(simpleType, typeExpr))(FuncType.apply _))

    //////
    // Member selection / call

    val memberOrCall: P[ValueExpr] = P {
      type MemberPart = (Int, String/*, Seq[TypeExpr]*/)
      type CallPart = ValueExpr
      type Part = Either[MemberPart, CallPart]

      def makeNode(startIndex: Int)(e: ValueExpr, partWithEndIndex: (Part, Int)): ValueExpr = {
        val (part, endIndex) = partWithEndIndex
        def posFrom(fromIndex: Int) = makePos(fromIndex, Some(endIndex))
        val pos = posFrom(startIndex)

        part match {
          case Left((dotIndex, name/*, typeArgs*/)) =>
            MemberSelection(e, NamedMember(name/*, typeArgs*/, posFrom(dotIndex)), pos)
          case Right(argument) =>
            Call(e, argument, pos)
        }
      }

      val member = P(Index ~ dot ~ nl.rep ~ name /*~ maybeTypeArgs*/).map(Left(_))
      val call = simple.map(Right(_))

      NoCut(Index ~ simple ~ ((member | call) ~ Index).rep(1)).map { case (startIndex, base, parts) =>
        val mk = makeNode(startIndex) _
        parts.tail.foldLeft(mk(base, parts.head))(mk)
      }
    }

    //////
    // Type parameters
    val plus = P("+")
    val minus = P("-")

    def typeListSyntax[T](item: => P[T]): P[Seq[T]] = P(bracketOpen ~ commaSeparatedRep(1, item) ~ bracketClose)

    // val typeParam: P[TypeParam] = P {
    //   val plusOrMinusVariance: P[Variance] = P(plus.map(_ => Covariant) | minus.map(_ => Contravariant))
    //   val variance = P(plusOrMinusVariance.?.map(_.getOrElse(Invariant)))

    //   val arity: P[Int] = typeListSyntax(underscore).?.map(_.map(_.length).getOrElse(0))

    //   pp(variance ~ name ~ arity)(TypeParam.apply _)
    // }

    val bracketOpen = "["
    val bracketClose = "]"

    // val maybeTypeParams: P[Seq[TypeParam]] = typeListSyntax(typeParam).?.map(_.getOrElse(Nil))

    // val maybeTypeArgs: P[Seq[TypeExpr]] = typeListSyntax(typeExpr).?.map(_.getOrElse(Nil))

    //////
    // Attributes
    val at = "@"

    val attribute: P[Attribute] = P(pp(at ~ bracketOpen ~ name ~ anythingBefore(bracketClose).!.?.map(_.filter(_.nonEmpty)) ~ bracketClose)(Attribute.apply _))

    val attributes = P(attribute.rep(sep=nl.rep) ~ nl.rep)

    //////
    // In-block defs
    val doubleColon = "::"
    def keyword(word: String) = doubleColon ~~ word

    val valueDecl: P[ValueDecl] = P(pp(keyword("declare") ~ keyword("val") ~/ name ~ typed)(ValueDecl.apply _))

    val valueNamesDef: P[ValueNamesDef] = P(pp(pattern ~ equalsSign ~/ nl.rep ~ valueExpr)(ValueNamesDef.apply _))

    val aliasTypeDef: P[AliasTypeDef] = P(pp(keyword("alias") ~/ name ~ equalsSign ~ typeExpr)(AliasTypeDef.apply _))

    val typeDef: P[TypeDef] = P(pp(
      keyword("declare").!.?.map(_.isDefined) ~
      keyword("type") ~/
      name ~ equalsSign ~ typeExpr
    )(TypeDef.apply _))

    val newExpr: P[NewExpr] = P(pp(keyword("new") ~/ namedType)(NewExpr.apply _))

    val methodDecl: P[MethodDecl] = P(pp(attributes ~ name ~ typed)(MethodDecl.apply _))

    val methodDef: P[MethodDef] = P(pp(attributes ~ name ~ typed.? ~ equalsSign ~ valueExpr)(MethodDef.apply _))

    val methodsBody: P[Seq[Method]] = P(recordSyntax(methodDef | methodDecl))

    val methodDeclSection: P[MethodDeclSection] = P(pp(
      keyword("declare") ~
      keyword("methods") ~/ typeExpr ~ methodsBody
    )(MethodDeclSection.apply _))

    val methodDefSection: P[MethodDefSection] = P(pp(
      keyword("methods") ~/ pattern ~ methodsBody
    )(MethodDefSection.apply _))

    val inBlockDef: P[InBlockDef] = P(valueDecl | valueNamesDef | aliasTypeDef | typeDef | methodDeclSection | methodDefSection)

    //////
    // Top level rules

    val valueExpr: P[ValueExpr] = P(memberOrCall | lambda | valueAs | simple).opaque("expression")

    val typeExpr: P[TypeExpr] = P(funcType | simpleType)

    val pattern: P[Pattern] = P(pp((unitPattern | tuplePattern | patternParens | namePattern) ~ typed.?)((p, t, pos) => t map (PatternAs(p, _, pos)) getOrElse p))

    //////
    // Start here

    val program = P(pp(blockContents(true) ~ End)(Block.apply _))
  }
}
