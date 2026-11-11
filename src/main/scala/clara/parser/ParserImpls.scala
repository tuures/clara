package clara.parser

import clara.ast.{Ast, AstLiteral, NoPos, Pos, SourcePos, SourceInfo}
import clara.util.Safe._

// TODO add more cuts to optimise and improve the error messages
// https://www.lihaoyi.com/fastparse/#Cuts
case class ParserImpls(sourceInfo: Option[SourceInfo]) {
  import Ast._
  import fastparse._

  val nlPred: Char => Boolean = (_: Char) === '\n'

  val space = " "

  def lineComment[X: P] = P("//" ~~ CharsWhile(c => !nlPred(c)).? ~~ (End | CharPred(nlPred)))

  def blockComment[X: P] = P {
    val (start, end) = ("/*", "*/")

    start ~~ (!end ~~ AnyChar).repX ~~ (End | end)
  }

  def comment[X: P] = P(lineComment | blockComment)

  def noSpaceWhitespace[X: P] = P(comment).repX

  def leastOneSpaceWhitespace[X: P] = P(comment.repX ~~ space.repX(1) ~~ (comment | space).repX)

  implicit object whitespace extends Whitespace {
    override def apply(ctx: P[_]): P[Unit] = {
      implicit val ctx0 = ctx

      P(comment | space).repX
    }
  }

  def makePos(fromIndex: Int, untilIndex: Option[Int]) =
    sourceInfo.map(SourcePos(_, fromIndex, untilIndex)).getOrElse(NoPos)

  def withPos[T, X: P](p: => P[T]): P[(T, Pos)] = P {
    (Index ~~ p ~~ Index).map { case (fromIndex, t, untilIndex) =>
      (t, makePos(fromIndex, Some(untilIndex)))
    }
  }

  // pp is GENERATED CODE:
  // List.range(1, 6).map { pArity =>
  //   val cs = List.range(0, pArity + 1).
  //     map(i => ('A' + i).toChar.toString)
  //
  //   val typeParams = (cs :+ "X: P").mkString("[", ", ", "]")
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
  def pp[A, X: P](p: => P[Unit])(f: Pos => A) =
    P(withPos(p).map(t => f(t._2)))
  def pp[A, B, X: P](p: => P[A])(f: (A, Pos) => B) =
    P(withPos(p).map(t => f(t._1, t._2)))
  def pp[A, B, C, X: P](p: => P[(A, B)])(f: (A, B, Pos) => C) =
    P(withPos(p).map(t => f(t._1._1, t._1._2, t._2)))
  def pp[A, B, C, D, X: P](p: => P[(A, B, C)])(f: (A, B, C, Pos) => D) =
    P(withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._2)))
  def pp[A, B, C, D, E, X: P](p: => P[(A, B, C, D)])(f: (A, B, C, D, Pos) => E) =
    P(withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._1._4, t._2)))
  def pp[A, B, C, D, E, F, X: P](p: => P[(A, B, C, D, E)])(f: (A, B, C, D, E, Pos) => F) =
    P(withPos(p).map(t => f(t._1._1, t._1._2, t._1._3, t._1._4, t._1._5, t._2)))

  case class Prefix[+A](fromIndex: Int, value: A)

  def startingWith[A, B, X: P](p: => P[A])(tails: Prefix[A] => P[B]): P[B] =
    P((Index ~~ p).flatMapX { case (fromPos, value) => tails(Prefix(fromPos, value)) })

  def ppTail[A, T, B, X: P](prefix: Prefix[A])(p: => P[T])(f: (A, T, Pos) => B): P[B] =
    P((Pass ~ p ~~ Index).map { case (t, untilIndex) =>
      f(prefix.value, t, makePos(prefix.fromIndex, Some(untilIndex)))
    })

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
    def binary[X: P] = P(withPrefixAndUnderscores("b", binaryDigits).map(AstLiteral.IntegerBin.apply _))

    def decimal[X: P] = P(decimalDigitsWithUnderscore.map(AstLiteral.IntegerDec.apply _))

    def hexDigits[X: P] = CharsWhileIn("0-9a-fA-F")
    def hex[X: P] = P(withPrefixAndUnderscores("x", hexDigits).map(AstLiteral.IntegerHex.apply _))

    def value[X: P]: P[AstLiteral.Integer] = P(binary | decimal | hex)
  }

  def integerLiteral[X: P] = P(pp(IntegerLiteralImpl.value)(IntegerLiteral.apply _))

  def integerPattern[X: P] = P(pp(IntegerLiteralImpl.value)(IntegerPattern.apply _))

  object FloatLiteralImpl {
    def n[X: P] = decimalDigitsWithUnderscore
    def value[X: P] = P((n ~~ dot ~~ n).map(AstLiteral.Float.tupled))
  }

  def floatLiteral[X: P] = P(pp(FloatLiteralImpl.value)(FloatLiteral.apply _))

  def floatPattern[X: P] = P(pp(FloatLiteralImpl.value)(FloatPattern.apply _))

  object ProcessedStringLiteralImpl {
    implicit class CharToString(c: Char) {
      def s = c.toString
    }

    val quote = '"'
    val escapeStart = '\\'
    val exprStart = '$'

    def boundary[X: P] = CharPred(_ === quote).opaque("quote")

    def plainPart[X: P]: P[AstLiteral.StringPlainPart] = P {
      val nothingSpecial = (c: Char) => c != quote && c != escapeStart && c != exprStart

      CharsWhile(nothingSpecial).!.map(AstLiteral.StringPlainPart(_))
    }

    def unicodeEscapeBody[X: P] = P("u" ~~ CharIn("0-9a-fA-F").repX(min=1, max=6))
    def escapeBody[X: P] = P(unicodeEscapeBody | quote.s | escapeStart.s | exprStart.s | "n" | "t" | "r")
    def escapePart[X: P]: P[AstLiteral.StringEscapePart] =
      P((escapeStart.s ~~ escapeBody.!).repX(1).map(AstLiteral.StringEscapePart(_)))

    def parensSyntax[T, X: P](p: => P[T]) = P(parensStart(p) ~ parensEnd)

    def valueExprPart[X: P]: P[AstLiteral.StringValueExprPart] =
      P((exprStart.s ~~ (parensSyntax(valueExpr) | namedValue)).map(AstLiteral.StringValueExprPart(_)))

    object StringPatternPartImpl {
      def withoutParens[X: P]: P[AstLiteral.StringPatternPart] =
        P(namedConstantPattern.map(AstLiteral.StringNamedConstantPart(_)) | capturePattern.map(AstLiteral.StringCapturePart(_)))
      def withParens[X: P]: P[AstLiteral.StringPatternPart] =
        P(parensSyntax(withoutParens))

      def withOrWithoutParens[X: P]: P[AstLiteral.StringPatternPart] =
        P(exprStart.s ~~ (withParens | withoutParens))
    }

    def valuePart[X: P]: P[AstLiteral.StringValuePart] = P(plainPart | escapePart | valueExprPart)
    def valueParts[X: P] = P(boundary ~~ valuePart.repX ~~ boundary)

    def patternPart[X: P]: P[AstLiteral.StringPatternPart] =
      P(plainPart | escapePart | StringPatternPartImpl.withOrWithoutParens)
    def patternParts[X: P] = P(boundary ~~ patternPart.repX ~~ boundary)
  }
  def processedStringValueParts[X: P] = P(ProcessedStringLiteralImpl.valueParts)
  def processedStringPatternParts[X: P] = P(ProcessedStringLiteralImpl.patternParts)

  object VerbatimStringLiteralImpl {
    val quote = '\''
    def boundary[X: P] = CharPred(_ === quote).opaque("quote")
    def startMarker[X: P]: P[Int] = P(hash.repX.!.map(_.length) ~~ boundary)
    def endMarker[X: P](hashCount: Int) = P(boundary ~~ hash.repX(exactly=hashCount))

    def plainPart[X: P] = P(startMarker.flatMapX { hashCount =>
      (anythingBefore(endMarker(hashCount)).! ~~ endMarker(hashCount)).map(s => if (s.isEmpty) Nil else Seq(AstLiteral.StringPlainPart(s)))
    })
  }
  def verbatimStringPlainPart[X: P] = P(VerbatimStringLiteralImpl.plainPart)

  def stringLiteral[X: P] = P(pp(processedStringValueParts | verbatimStringPlainPart)(StringLiteral.apply _))
  def stringPattern[X: P] = P(pp(processedStringPatternParts | verbatimStringPlainPart)(StringPattern.apply _))

  //////
  // Parens, tuples, block

  val openParens = "("
  val closeParens = ")"
  val semi = ";"

  def parensStart[T, X: P](p: => P[T]) = P(openParens ~ nl.rep ~ p)
  def parensEnd[X: P] = P(nl.rep ~ closeParens)

  def blockItemSep[X: P] = P(nl | semi)

  def blockContent[X: P] = P(inBlockDecl | valueExpr)

  def blockItems[X: P](min: Int): P[Seq[BlockContent]] =
    P(blockContent.rep(min=min, sep=blockItemSep.rep(1)) ~ blockItemSep.rep)

  def blockTail[X: P](prefix: Prefix[BlockContent]): P[ValueExpr] =
    ppTail(prefix)((blockItemSep.rep(1) ~ blockItems(0)).? ~ closeParens) { (first, rest, pos) =>
      Block(first +: rest.getOrElse(Nil), pos)
    }

  def tupleTail[T, X: P](element: => P[T], prefix: Prefix[T])(make: (Seq[T], Pos) => T): P[T] =
    ppTail(prefix)(comma ~ commaSeparatedRep(1, element) ~ closeParens) { (first, rest, pos) =>
      make(first +: rest, pos)
    }

  def parensTail[T, X: P](prefix: Prefix[T]): P[T] = P((Pass ~ parensEnd).map(_ => prefix.value))

  def tupleOrParensTail[T, X: P](element: => P[T], prefix: Prefix[T])(make: (Seq[T], Pos) => T): P[T] =
    P(tupleTail(element, prefix)(make) | parensTail(prefix))

  def blockWithLeadingSemi[X: P]: P[Block] =
    P(pp(parensStart(semi ~ blockItemSep.rep ~ blockItems(1)) ~ closeParens)(Block.apply _))

  def tupleOrParensOrBlock[X: P]: P[ValueExpr] = P {
    startingWith(parensStart(inBlockDecl))(blockTail(_)) |
    startingWith(parensStart(valueExpr)) { prefix =>
      tupleOrParensTail(valueExpr, prefix)(Tuple.apply _) | blockTail(prefix)
    } |
    blockWithLeadingSemi
  }

  def tupleOrParensSyntax[T, X: P](element: => P[T])(make: (Seq[T], Pos) => T): P[T] = P {
    startingWith(parensStart(element))(tupleOrParensTail(element, _)(make))
  }

  def tupleOrParensType[X: P]: P[TypeExpr] = P(tupleOrParensSyntax(typeExpr)(TupleType.apply _))

  def tupleOrParensPattern[X: P]: P[Pattern] = P(tupleOrParensSyntax(pattern)(TuplePattern.apply _))

  //////
  // Names

  // NOTE: Character.isLetter* works only with BMP characters
  def name[X: P]: P[String] =
    ((underscore | CharPred(Character.isLetter(_))) ~~ CharsWhile(Character.isLetterOrDigit(_)).?).!.opaque("name")

  def nameWithPos[X: P]: P[NameWithPos] = P(pp(name)(NameWithPos.apply _))

  def namedValue[X: P]: P[NamedValue] = P(pp(name)(NamedValue.apply _))

  def nsPath[X: P]: P[Seq[NameWithPos]] = P((nameWithPos ~~ dot).repX)

  def namedType[X: P]: P[NamedType] = P(pp(nsPath ~~ nameWithPos ~ maybeTypeArgs)(NamedType.apply _))

  // NOTE: Character.isUpperCase works only with BMP characters
  def anyUpperCaseChar[X: P]: P[Unit] = P(CharPred(Character.isUpperCase(_)))

  def namedConstantPattern[X: P]: P[NamedConstantPattern] =
    P(pp(&(anyUpperCaseChar) ~~ nsPath ~~ nameWithPos)(NamedConstantPattern.apply _))

  def capturePattern[X: P]: P[CapturePattern] =
    P(pp(/*!(anyUpperCaseChar) ~~ */name)(CapturePattern.apply _))

  //////
  // ValueAs

  val colon = ":"

  def typed[X: P]: P[TypeExpr] = P(colon ~ nl.rep ~ typeExpr)

  def valueAsTail[X: P](prefix: Prefix[ValueExpr]): P[ValueExpr] = ppTail(prefix)(typed)(ValueAs.apply _)

  def patternAsTail[X: P](prefix: Prefix[Pattern]): P[Pattern] = ppTail(prefix)(typed)(PatternAs.apply _)

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

  def fieldPattern[X: P] = P(pp(name ~ typed.? ~ (equalsSign ~ pattern).?)(FieldPattern.apply _))
  def recordPattern[X: P] = P(pp(recordSyntax(fieldPattern))(RecordPattern.apply _))

  //////
  // Simple

  def simple[X: P]: P[ValueExpr] =
    P(unitLiteral | floatLiteral | integerLiteral | stringLiteral | tupleOrParensOrBlock | namedValue | record | rawConstructor)

  def simpleType[X: P]: P[TypeExpr] =
    P(topType | bottomType | unitType | tupleOrParensType | namedType | recordType)

  def wildcardPattern[X: P]: P[WildcardPattern] =
    P(pp("_" ~~ &(End | CharPred(c => !Character.isLetterOrDigit(c))))(WildcardPattern.apply _))

  def simplePattern[X: P]: P[Pattern] =
    P(wildcardPattern | unitPattern | floatPattern | integerPattern | stringPattern | tupleOrParensPattern | recordPattern | namedConstantPattern | capturePattern)

  def compoundPattern[X: P]: P[Pattern] = P(constructPattern | simplePattern)

  //////
  // Unions and intersections
  val verticalBar = "|"
  val ampersand = "&"

  def unionTypeTail[X: P](prefix: Prefix[TypeExpr]): P[TypeExpr] =
    ppTail(prefix)((verticalBar ~ simpleType).rep(1))((first, rest, pos) => UnionType(first +: rest, pos))

  def intersectionTypeTail[X: P](prefix: Prefix[TypeExpr]): P[TypeExpr] =
    ppTail(prefix)((ampersand ~ simpleType).rep(1))((first, rest, pos) => IntersectionType(first +: rest, pos))

  def orPatternTail[X: P](prefix: Prefix[Pattern]): P[Pattern] =
    ppTail(prefix)((verticalBar ~ compoundPattern).rep(1))((first, rest, pos) => OrPattern(first +: rest, pos))

  //////
  // Function syntax

  val funcArrow = "=>"

  def funcSyntax[T1, T2, X: P](in: => P[T1], out: => P[T2]) = P(in ~ funcArrow ~ nl.rep ~ out)
  def polyFuncSyntax[T1, T2, X: P](in: => P[T1], out: => P[T2]) =
    P(maybeTypeParams ~ funcSyntax(in, out)).map { case (p, (in, out)) => (p, in, out) }

  def lambda[X: P]: P[Lambda] = P(pp(polyFuncSyntax(simplePattern, valueExpr))(Lambda.apply _))

  object PiecewiseImpl {
    def piece[X: P] = P(funcSyntax(pattern, valueExpr))
    def pieceSep[X: P] = P((nl | comma) ~ nl.rep) // FIXME duplication with recordItemSep
    def pieces[X: P] = P(piece.rep(sep=pieceSep))
  }

  def piecewise[X: P]: P[Piecewise] = {
    import PiecewiseImpl._

    P(pp(hash ~ parensStart(pieces) ~ comma.? ~ parensEnd)(Piecewise.apply _))
  }

  def funcTypeTail[X: P](prefix: Prefix[TypeExpr]): P[TypeExpr] =
    ppTail(prefix)(funcArrow ~ nl.rep ~ typeExpr)(FuncType(Nil, _, _, _))

  def polyFuncType[X: P]: P[FuncType] = P(pp(&(angleOpen) ~~ polyFuncSyntax(simpleType, typeExpr))(FuncType.apply _))

  //////
  // Member selection / call

  object MemberOrJuxtaCallImpl {
    type Part = Either[NamedValue, ValueExpr]

    def makeNode(prevNode: ValueExpr, nextPart: Part) = nextPart match {
      case Left(namedValue) => MemberSelection(prevNode, namedValue, prevNode.pos.join(namedValue.pos))
      case Right(argument) => Call(prevNode, argument, prevNode.pos.join(argument.pos))
    }

    def memberPart[X: P] = P(space.rep ~ nl.? ~ dot ~ nl.rep ~ namedValue).map(Left(_))
    def juxtaCallPart[X: P] = P(simple.map(Right(_)))

    def tail[X: P](base: ValueExpr): P[ValueExpr] =
      P((noSpaceWhitespace ~~ (memberPart | juxtaCallPart).repX(1)).?).map { parts =>
        parts.getOrElse(Nil).foldLeft(base)(makeNode(_, _))
      }
  }

  def spaceCallPart[X: P]: P[ValueExpr] = P(simple.flatMapX(MemberOrJuxtaCallImpl.tail(_)))

  def spaceCallTail[X: P](callee: ValueExpr): P[ValueExpr] = P {
    def makeNode(callee: ValueExpr, argument: ValueExpr) = {
      Call(callee, argument, callee.pos.join(argument.pos))
    }

    (leastOneSpaceWhitespace ~~ spaceCallPart).repX.map(_.foldLeft(callee)(makeNode(_, _)))
  }

  def spaceCall[X: P]: P[ValueExpr] = P(spaceCallPart.flatMapX(spaceCallTail(_)))

  def valueAsOrSpaceCall[X: P]: P[ValueExpr] = P {
    startingWith(simple) { prefix =>
      valueAsTail(prefix) | MemberOrJuxtaCallImpl.tail(prefix.value).flatMapX(spaceCallTail(_))
    }
  }

  def pipePart[X: P]: P[ValueExpr] = P(piecewise | lambda | valueAsOrSpaceCall)
  def pipeSep[X: P] = P(nl.? ~ at ~ nl.rep)

  def rawConstructor[X: P]: P[RawConstructor] = P(pp(keyword("new") ~ (nsPath ~~ nameWithPos))(RawConstructor.apply _))

  // TODO: should we allow also lowercase name here?
  def constructPattern[X: P] = P(pp(&(anyUpperCaseChar) ~~ nameWithPos ~ simplePattern)(ConstructPattern.apply _))

  val questionMark = "?"
  val doubleQuestionMark = "??"

  def patternEmbeddedExpression[X: P]: P[ValueExpr] = P(spaceCall)

  def guardPatternTail[X: P](prefix: Prefix[Pattern]): P[Pattern] =
    ppTail(prefix)(questionMark ~ nl.rep ~ patternEmbeddedExpression)(GuardPattern.apply _)

  def defaultValuePatternTail[X: P](prefix: Prefix[Pattern]): P[Pattern] =
    ppTail(prefix)(doubleQuestionMark ~ nl.rep ~ patternEmbeddedExpression)(DefaultValuePattern.apply _)

  //////
  // Type parameters
  // val plus = P("+")
  // val minus = P("-")

  val angleOpen = "<"
  val angleClose = ">"

  def typeListSyntax[T, X: P](item: => P[T]): P[Seq[T]] = P(angleOpen ~ commaSeparatedRep(1, item) ~ angleClose)

  def typeParam[X: P]: P[TypeParam] = P {
    // TODO variance
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

  def attributes[X: P]: P[Seq[Attribute]] = P(attribute.rep(sep=nl.rep) ~ nl.rep)

  //////
  // In-block defs
  val doubleColon = "::"
  def keywordSyntax[T, X: P](word: => P[T]): P[T] = P(doubleColon ~~ word ~ nl.rep)

  def keyword[X: P](word: String): P[Unit] = P(keywordSyntax(word))

  object TypeDefImpl {
    def alias[X: P] = P("alias").map(_ => TypeDefKind.Alias)
    def tagged[X: P] = P("tagged").map(_ => TypeDefKind.Tagged)
    def boxed[X: P] = P("boxed").map(_ => TypeDefKind.Boxed)
    def opaque[X: P] = P("opaque").map(_ => TypeDefKind.Opaque)
    def singleton[X: P] = P("singleton").map(_ => TypeDefKind.Singleton)

    def typeDefKind[X: P] = P(keywordSyntax(alias | tagged | boxed | opaque | singleton))
  }

  def declTargetType[X: P]: P[DeclTargetType] = P(pp(nameWithPos ~ maybeTypeParams)(DeclTargetType.apply _))

  def typeDef[X: P]: P[TypeDef] = P(pp(
    TypeDefImpl.typeDefKind ~
    declTargetType ~ typed.?
  )(TypeDef.apply _))

  def namespaceMember[X: P]: P[InBlockDecl] = P(typeDef | namespaceDef | valueDecl | valueDef)

  def extendMember[X: P]: P[InBlockDecl] = P(methodDeclSection | methodDefSection | namespaceMember)

  def namespaceDef[X: P]: P[NamespaceDef] = P(pp(
    keyword("namespace").map(_ => false) ~ nameWithPos ~ recordSyntax(namespaceMember) |
    keyword("extend").map(_ => true) ~ nameWithPos ~ recordSyntax(extendMember)
  )(NamespaceDef.apply _))

  def methodDecl[X: P]: P[MethodDecl] = P(pp(attributes ~ nameWithPos ~ typed)(MethodDecl.apply _))

  def methodDef[X: P]: P[MethodDef] = P(pp(attributes ~ nameWithPos ~ typed.? ~ equalsSign ~ valueExpr)(MethodDef.apply _))

  def methodDeclSection[X: P]: P[MethodDeclSection] = P(pp(
    keyword("declare") ~ keyword("methods") ~/ maybeTypeParams ~ recordSyntax(methodDecl)
  )(MethodDeclSection.apply _))

  def methodDefSection[X: P]: P[MethodDefSection] = P(pp(
    keyword("methods") ~/ maybeTypeParams ~ simplePattern ~ recordSyntax(methodDef)
  )(MethodDefSection.apply _))

  def valueDecl[X: P]: P[ValueDecl] = P(pp(keyword("declare") ~ nameWithPos ~ typed)(ValueDecl.apply _))

  def valueDef[X: P]: P[ValueDef] = P(pp(pattern ~ equalsSign ~ nl.rep ~ valueExpr)(ValueDef.apply _))

  def inBlockDecl[X: P]: P[InBlockDecl] = P(typeDef | namespaceDef | valueDecl | valueDef)

  //////
  // Top level rules

  def valueExpr[X: P]: P[ValueExpr] = P {
    def makeNode(argument: ValueExpr, callee: ValueExpr) = {
      Pipe(argument, callee, argument.pos.join(callee.pos))
    }

    pipePart.rep(1, sep=pipeSep).map(parts => parts.tail.foldLeft(parts.head)(makeNode(_, _)))
  }

  def typeExpr[X: P]: P[TypeExpr] = P {
    polyFuncType | startingWith(simpleType) { prefix =>
      funcTypeTail(prefix) | unionTypeTail(prefix) | intersectionTypeTail(prefix) | Pass(prefix.value)
    }
  }

  def compoundPatternTail[X: P](prefix: Prefix[Pattern]): P[Pattern] =
    P(defaultValuePatternTail(prefix) | guardPatternTail(prefix) | orPatternTail(prefix) | Pass(prefix.value))

  def pattern[X: P]: P[Pattern] = P {
    startingWith(constructPattern)(compoundPatternTail(_)) | startingWith(simplePattern) { prefix =>
      patternAsTail(prefix) | compoundPatternTail(prefix)
    }
  }

  //////
  // Start here

  def programBlock[X: P]: P[Block] = P(pp(Start ~ blockItemSep.rep ~ blockItems(0) ~ End)(Block.apply _))
}
