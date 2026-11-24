package clara

import clara.analyzer.Analyzer
import clara.ast.Ast
import clara.jsemitter.JsEmitter
import clara.jsemitter.impl.JsPrinter
import clara.parser.Parser
import clara.util.{FileIo, Message}

object Compiler {

  def compileFile(inputPath: String): Either[Seq[Message], (String, Seq[Message])] =
    Impl.parseFile(inputPath).flatMap(Impl.analyzeAndEmit)

  object Impl {
    def parseFile(inputPath: String): Either[Seq[Message], Ast.Block] =
      FileIo.readFile(inputPath).flatMap { input =>
        Parser.parseString(inputPath, input)
      }

    def analyzeAndEmit(moduleBlock: Ast.Block): Either[Seq[Message], (String, Seq[Message])] = {
      val analyzed = Analyzer.analyzeModule(moduleBlock)

      analyzed.module.toRight(analyzed.messages).map { module =>
        val jsAst = JsEmitter.emitModule(module)
        (JsPrinter.printModule(jsAst), analyzed.messages)
      }
    }
  }

}
