package clara.parser

import clara.ast.{Ast, SourceInfo}
import clara.util.Safe._

import fastparse.{parse, Parsed}

object Parser {
  def parseString(sourceName: String, input: String): Ast.Block = {
    val sourceInfo = SourceInfo.fromString(sourceName, input)

    parse(input, ParserImpls(Some(sourceInfo)).programBlock(_)) match {
      case Parsed.Success(block, index) => {
        if(index !== sourceInfo.length) {
          throw new IllegalArgumentException("Input not fully parsed")
        }

        block
      }
      case Parsed.Failure(_, _, extra) => {
        throw new IllegalArgumentException(safe"Parser failed to handle input: ${extra.trace().longMsg}")
      }
    }
  }
}
