package clara.parser

import clara.ast.{Ast, SourceInfo, SourcePos, SourceMessage}
import clara.util.Safe._

import fastparse.{parse, Parsed}

object Parser {
  def parseString(sourceName: String, input: String): Either[Seq[SourceMessage], Ast.Block] = {
    val sourceInfo = SourceInfo.fromString(sourceName, input)

    parse(input, ParserImpls(Some(sourceInfo)).programBlock(_)) match {
      case Parsed.Success(block, index) => {
        assert(index === sourceInfo.length, "Parser did not fully consume the input")

        Right(block)
      }
      case f: Parsed.Failure =>
        val pos = SourcePos(sourceInfo, f.index, None)
        val message = "Syntax error: expected " + f.trace(true).terminals
        Left(Seq(SourceMessage(pos, message)))
    }
  }
}
