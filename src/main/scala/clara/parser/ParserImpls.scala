package clara.parser

import clara.ast.{Ast, LiteralValue, NoPos, Pos, SourcePos, SourceInfo}
import clara.util.Safe._

// TODO add more cuts to optimise and improve the error messages
// https://www.lihaoyi.com/fastparse/#Cuts
case class ParserImpls(sourceInfo: Option[SourceInfo]) {
  import Ast._
  import fastparse._

  val nlPred: Char => Boolean = (_: Char) === '\n'

  implicit object whitespace extends Whitespace {
    override def apply(ctx: P[_]): P[Unit] = {
      implicit val ctx0 = ctx

      def lineComment[X: P] = P("//" ~~ CharsWhile(c => !nlPred(c)).? ~~ (End | CharPred(nlPred)))

      def blockComment[X: P] = P {
        val (start, end) = ("/*", "*/")

        start ~~ (!end ~~ AnyChar).repX ~~ (End | end)
      }

      val space = " "

      def comment[X: P] = P(lineComment | blockComment)

      P(comment | space).repX
    }
  }

  def makePos(from: Int, until: Option[Int]) =
    sourceInfo.map(SourcePos(_, from, until)).getOrElse(NoPos)

  def withPos[T, X: P](p: => P[T]): P[(T, Pos)] = P {
    (Index ~ p ~ Index).map { case (from, t, until) =>
      (t, makePos(from, Some(until)))
    }
  }

  // pp is GENERATED CODE:
  // List.range(1, 5).map { pArity =>
  //   val cs = List.range(0, pArity + 1).
  //     map(i => ('A' + i).toChar.toString)
  //
  //   val typeParams = (cs ++ "X: P").mkString("[", ", ", "]")
  //
  //   val tupleArgs = cs.init.mkString(", ")
  //   val pArg =
  //     if (pArity == 1) cs.head
  //     else safe"($tupleArgs)"
  //   val valueParams = safe"(p: => P[$pArg])(f: ($tupleArgs, Pos) => ${cs.last})"
  //
  //   val tupleValues = (
  //     if (pArity == 1) Seq("t._1")
  //     else List.range(1, pArity + 1).map(i => safe"t._1._$i")
  //   ).mkString(", ")
  //   val impl = safe"P(withPos(p).map(t => f($tupleValues, t._2)))"
  //
  //   safe"def pp$typeParams$valueParams = $impl"
  // }.foreach(println)
  def pp[A, X: P](p: => P[Unit])(f: Pos => A) = P(withPos(p).map(t => f(t._2)))
  def pp[A, B, X: P](p: => P[A])(f: (A, Pos) => B) = P(withPos(p).map(t => f(t._1, t._2)))
  def pp[A, B, C, X: P](p: => P[(A, B)])(f: (A, B, Pos) => C) = P(withPos(p).map(t => f(t._1._1, t._1._2, t._2)))
  def pp[A, B, C, D, X: P](p: => P[(A, B, C)])(f: (A, B, C, Pos) => D) = P(withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._2)))
  def pp[A, B, C, D, E, X: P](p: => P[(A, B, C, D)])(f: (A, B, C, D, Pos) => E) = P(withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._1._4, t._2)))

  //////
  // Basics

  def nl[X: P] = P(CharPred(nlPred)).opaque("newline")

  val comma = ","

  def commaSeparatedRep[T, X: P](min: Int, p: => P[T]) = P(nl.rep ~ p.rep(min, sep=(comma ~ nl.rep)) ~ comma.? ~ nl.rep)

  def anythingBefore[T, X: P](end: => P[T]) = P((!end ~~ AnyChar).repX)

  val dot = "."

  val underscore = "_"

  //////
  // Literals

  val topTypeSyntax = "*"
  val bottomTypeSyntax = "!"

  def topType[X: P]: P[TopType] = P(pp(topTypeSyntax)(TopType.apply _))

  def bottomType[X: P]: P[BottomType] = P(pp(bottomTypeSyntax)(BottomType.apply _))

  val unitSyntax = "()"

  def unitLiteral[X: P]: P[UnitLiteral] = P(pp(unitSyntax)(UnitLiteral.apply _))

  def unitType[X: P]: P[UnitType] = P(pp(unitSyntax)(UnitType.apply _))

  def unitPattern[X: P]: P[UnitPattern] = P(pp(unitSyntax)(UnitPattern.apply _))

  val hash = "#"

  def withUnderscores[A, X: P](digits: => P[A]): P[String] = P {
    (digits.! ~~ (underscore ~ nl.rep ~ digits.!).repX).map { case (head, tail) =>
      head + tail.foldLeft("")(_ + _)
    }
  }

  def decimalDigits[X: P] = CharsWhileIn("0-9")
  def decimalDigitsWithUnderscore[X: P] = P(withUnderscores(decimalDigits))

  object IntegerLiteralImpl {
    def withPrefixAndUnderscores[A, X: P](prefix: String, digits: => P[A]): P[String] =
      P((hash + prefix) ~~ withUnderscores(digits))

    def binaryDigits[X: P] = CharsWhileIn("0-1")
    def binary[X: P] = P(withPrefixAndUnderscores("b", binaryDigits).map(LiteralValue.IntegerBin.apply _))

    def decimal[X: P] = P(decimalDigitsWithUnderscore.map(LiteralValue.IntegerDec.apply _))

    def hexDigits[X: P] = P((decimalDigits | CharIn("A-F") | CharIn("a-f")).repX(1))
    def hex[X: P] = P(withPrefixAndUnderscores("x", hexDigits).map(LiteralValue.IntegerHex.apply _))

    def value[X: P]: P[LiteralValue.Integer] = P(binary | decimal | hex)
  }

  def integerLiteral[X: P] = P(pp(IntegerLiteralImpl.value)(IntegerLiteral.apply _))

  object FloatLiteralImpl {
    def n[X: P] = decimalDigitsWithUnderscore
    def value[X: P] = P((n ~~ dot ~~ n).map(LiteralValue.Float.tupled))
  }

  def floatLiteral[X: P] = P(pp(FloatLiteralImpl.value)(FloatLiteral.apply _))

  object ProcessedStringLiteralImpl {
    implicit class CharToString(c: Char) {
      def s = c.toString
    }

    val quote = '"'
    val escapeStart = '\\'
    val exprStart = '$'

    def boundary[X: P] = CharPred(_ === quote).opaque("quote")

    def plainPart[X: P]: P[LiteralValue.StringPlainPart] = P {
      val nothingSpecial = (c: Char) => c != quote && c != escapeStart && c != exprStart

      CharsWhile(nothingSpecial).!.map(LiteralValue.StringPlainPart(_))
    }

    // TODO use StringIn for better performance
    def escapeBody[X: P] = P(quote.s | escapeStart.s | exprStart.s | "n" | "t")
    def escapePart[X: P]: P[LiteralValue.StringEscapePart] =
      P((escapeStart.s ~~ escapeBody.!).repX(1).map(LiteralValue.StringEscapePart(_)))

    def exprPart[X: P]: P[LiteralValue.StringExpressionPart] =
      P((exprStart.s ~~ (parens | namedValue)).map(LiteralValue.StringExpressionPart(_)))

    def part[X: P]: P[LiteralValue.StringPart] = P(plainPart | escapePart | exprPart)

    def parts[X: P] = P(boundary ~~ part.repX(1) ~~ boundary)
  }

  def processedStringLiteral[X: P] = P(pp(ProcessedStringLiteralImpl.parts)(StringLiteral.apply _))

  object VerbatimStringLiteralImpl {
    val quote = '\''
    def boundary[X: P] = CharPred(_ === quote).opaque("quote")
    def startMarker[X: P]: P[Int] = P(hash.repX.!.map(_.length) ~~ boundary)
    def endMarker[X: P](hashCount: Int) = P(boundary ~~ hash.repX(exactly=hashCount))

    def valueBetweenMarkers[X: P] = P(startMarker.flatMapX { hashCount =>
      (anythingBefore(endMarker(hashCount)).! ~~ endMarker(hashCount)).map(s => Seq(LiteralValue.StringPlainPart(s)))
    })
  }
  def verbatimStringLiteral[X: P] = P(pp(VerbatimStringLiteralImpl.valueBetweenMarkers)(StringLiteral.apply _))

  def stringLiteral[X: P] = P(processedStringLiteral | verbatimStringLiteral)

  //////
  // Tuples
  val openParens = "("
  val closeParens = ")"

  def tupleSyntax[T, X: P](element: => P[T]) = P(openParens ~ commaSeparatedRep(2, element) ~ closeParens)

  def tuple[X: P]: P[Tuple] = P(pp(tupleSyntax(valueExpr))(Tuple.apply _))

  def tupleType[X: P]: P[TupleType] = P(pp(tupleSyntax(typeExpr))(TupleType.apply _))

  def tuplePattern[X: P]: P[TuplePattern] = P(pp(tupleSyntax(pattern))(TuplePattern.apply _))

  //////
  // Parens

  def parensSyntax[T, X: P](p: => P[T]) = P(openParens ~ nl.rep ~ p ~ nl.rep ~ closeParens)

  def parens[X: P]: P[ValueExpr] = P(parensSyntax(valueExpr))

  def typeParens[X: P]: P[TypeExpr] = P(parensSyntax(typeExpr))

  def patternParens[X: P]: P[Pattern] = P(parensSyntax(pattern))

  //////
  // Blocks

  val semi = ";"

  def blockItemSep[X: P] = P(nl | semi)

  def blockContents[X: P](isProgramBlock: Boolean): P[Seq[BlockContent]] = P {
    val min = if (isProgramBlock) 1 else 2

    def blockContent = P(inBlockDecl | valueExpr)

    P(blockItemSep.rep ~ blockContent.rep(min=min, sep=blockItemSep.rep(1)) ~ blockItemSep.rep)
  }

  def block[X: P]: P[Block] = P(pp(openParens ~ blockContents(false) ~ closeParens)(Block.apply _))

  //////
  // Names

  // FIXME allow numbers and other symbols in name
  def name[X: P] = CharsWhile(c => Character.isLetter(c)).!.opaque("name") // NOTE: Character.isLetter works only with BMP characters

  def nameWithPos[X: P]: P[NameWithPos] = P(pp(name)(NameWithPos.apply _))

  def namedValue[X: P]: P[NamedValue] = P(pp(name)(NamedValue.apply _))

  def namedType[X: P]: P[NamedType] = P(pp(nameWithPos ~ maybeTypeArgs)(NamedType.apply _))

  def namePattern[X: P]: P[NamePattern] = P(pp(name)(NamePattern.apply _))

  //////
  // ValueAs

  val colon = ":"

  def typed[X: P]: P[TypeExpr] = P(colon ~ nl.rep ~ typeExpr)

  // TODO is NoCut really needed? review all NoCuts
  def valueAs[X: P]: P[ValueAs] = P(pp(NoCut(simple ~ typed))(ValueAs.apply _))

  def patternAs[X: P]: P[PatternAs] = P(pp(simplePattern ~ typed)(PatternAs.apply _))

  //////
  // Record

  val braceOpen = "{"
  val braceClose = "}"
  val equalsSign = "="

  def recordItemSep[X: P] = P((nl | comma) ~ nl.rep)
  def recordSyntax[T, X: P](field: => P[T]) = P {
    braceOpen ~ nl.rep ~ field.rep(sep=recordItemSep) ~ comma.? ~ nl.rep ~ braceClose
  }

  def fieldDef[X: P] = P(pp(name ~ typed.? ~ equalsSign ~ valueExpr)(FieldDef.apply _))
  def record[X: P] = P(pp(recordSyntax(fieldDef))(Record.apply _))

  def fieldDecl[X: P] = P(pp(name ~ typed)(FieldDecl.apply _))
  def recordType[X: P] = P(pp(recordSyntax(fieldDecl))(RecordType.apply _))

  //////
  // Simple

  def simple[X: P]: P[ValueExpr] = P(unitLiteral | floatLiteral | integerLiteral | stringLiteral | tuple | block | parens | namedValue | record)

  def simpleType[X: P]: P[TypeExpr] = P(topType | bottomType | unitType | tupleType | typeParens | namedType | recordType)

  // FIXME floatPattern integerPattern stringPattern
  def simplePattern[X: P]: P[Pattern] = P(unitPattern | tuplePattern | patternParens | namePattern)

  //////
  // Function syntax

  val funcArrow = "=>"

  def funcSyntax[T1, T2, X: P](in: => P[T1], out: => P[T2]) = P(in ~ funcArrow ~ nl.rep ~ out)

  def lambda[X: P]: P[Lambda] = P(pp(funcSyntax(pattern, valueExpr))(Lambda.apply _))

  def funcType[X: P]: P[FuncType] = P(pp(funcSyntax(simpleType, typeExpr))(FuncType.apply _))

  //////
  // Member selection / call

  object MemberOrCallImpl {
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

    def memberPart[X: P] = P(Index ~ dot ~ nl.rep ~ name /*~ maybeTypeArgs*/).map(Left(_))
    def callPart[X: P] = simple.map(Right(_))

    def part[X: P] = P((memberPart | callPart) ~ Index)
    def oneOrMoreParts[X: P] = P(part.rep(1))

    def impl[X: P] = NoCut(Index ~ simple ~ oneOrMoreParts).map { case (startIndex, base, parts) =>
      val mk = makeNode(startIndex) _
      parts.tail.foldLeft(mk(base, parts.head))(mk)
    }
  }

  def memberOrCall[X: P]: P[ValueExpr] = P(MemberOrCallImpl.impl)

  //////
  // Type parameters
  // val plus = P("+")
  // val minus = P("-")

  val angleOpen = "<"
  val angleClose = ">"

  def typeListSyntax[T, X: P](item: => P[T]): P[Seq[T]] = P(angleOpen ~ commaSeparatedRep(1, item) ~ angleClose)

  def typeParam[X: P]: P[TypeParam] = P {
    // val plusOrMinusVariance: P[Variance] = P(plus.map(_ => Covariant) | minus.map(_ => Contravariant))
    // val variance = P(plusOrMinusVariance.?.map(_.getOrElse(Invariant)))

    // val arity: P[Int] = typeListSyntax(underscore).?.map(_.map(_.length).getOrElse(0))

    pp(/*variance ~ */name/* ~ arity*/)(TypeParam.apply _)
  }

  def maybeTypeParams[X: P]: P[Seq[TypeParam]] = P(typeListSyntax(typeParam).?.map(_.getOrElse(Nil)))

  def maybeTypeArgs[X: P]: P[Seq[TypeExpr]] = P(typeListSyntax(typeExpr).?.map(_.getOrElse(Nil)))

  //////
  // Attributes
  val at = "@"
  val bracketOpen = "["
  val bracketClose = "]"

  def attribute[X: P]: P[Attribute] = P(pp(at ~ bracketOpen ~ name ~ anythingBefore(bracketClose).!.?.map(_.filter(_.nonEmpty)) ~ bracketClose)(Attribute.apply _))

  def attributes[X: P] = P(attribute.rep(sep=nl.rep) ~ nl.rep)

  //////
  // In-block defs
  val doubleColon = "::"
  def keywordSyntax[T, X: P](word: => P[T]): P[T] = P(doubleColon ~/ word ~ nl.rep)

  def keyword[X: P](word: String) = P(keywordSyntax(word))

  object TypeDefImpl {
    def alias[X: P] = P("alias").map(_ => TypeDefKind.Alias)
    def tagged[X: P] = P("tagged").map(_ => TypeDefKind.Tagged)
    def boxed[X: P] = P("boxed").map(_ => TypeDefKind.Boxed)
    def opaque[X: P] = P("opaque").map(_ => TypeDefKind.Opaque)
    def singleton[X: P] = P("singleton").map(_ => TypeDefKind.Singleton)

    def typeDefKind[X: P] = P(keywordSyntax(alias | tagged | boxed | opaque | singleton))
  }

  def typeDef[X: P]: P[TypeDef] = P(pp(
    TypeDefImpl.typeDefKind ~
    nameWithPos ~ maybeTypeParams ~ typed.?
  )(TypeDef.apply _))

  def methodDef[X: P]: P[MethodDef] = P(pp(attributes ~ name ~ typed.? ~ equalsSign ~ valueExpr)(MethodDef.apply _))

  def methodDecl[X: P]: P[MethodDecl] = P(pp(attributes ~ name ~ typed)(MethodDecl.apply _))

  def methodsBody[X: P]: P[Seq[Method]] = P(recordSyntax(methodDef | methodDecl))

  // TODO use ConstructPattern instead of typeName ~ simplePattern
  def methodDefSection[X: P]: P[MethodDefSection] = P(pp(
    keyword("methods") ~/ nameWithPos ~ simplePattern ~ colon ~ nl.rep ~ methodsBody
  )(MethodDefSection.apply _))

  def methodDeclSection[X: P]: P[MethodDeclSection] = P(pp(
    keyword("declare") ~
    keyword("methods") ~/ nameWithPos ~ colon ~ nl.rep ~ methodsBody
  )(MethodDeclSection.apply _))

  def valueDecl[X: P]: P[ValueDecl] = P(pp(keyword("declare") ~ name ~ typed)(ValueDecl.apply _))

  def valueDef[X: P]: P[ValueDef] = P(pp(pattern ~ equalsSign ~/ nl.rep ~ valueExpr)(ValueDef.apply _))

  def inBlockDecl[X: P]: P[InBlockDecl] = P(typeDef | methodDefSection | methodDeclSection | valueDecl | valueDef)

  //////
  // Top level rules

  def valueExpr[X: P]: P[ValueExpr] = P(memberOrCall | lambda | valueAs | simple)

  def typeExpr[X: P]: P[TypeExpr] = P(funcType | simpleType)

  def pattern[X: P]: P[Pattern] = P(patternAs | simplePattern)

  //////
  // Start here

  def programBlock[X: P] = P(pp(blockContents(true) ~ End)(Block.apply _))
}
