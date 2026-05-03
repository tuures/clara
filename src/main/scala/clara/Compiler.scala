package clara

import clara.analyzer.Analyzer
import clara.ast.{Ast, AstPrinter}
import clara.jsemitter.JsEmitter
import clara.jsemitter.impl.JsPrinter
import clara.parser.Parser
import clara.util.{FileIo, Message}

object Compiler {

  def compileFile(inputPath: String): Either[Seq[Message], (String, Seq[Message])] =
    Impl.parseFile(inputPath).flatMap(Impl.analyzeAndEmit)

  def dumpAst(inputPath: String): Either[Seq[Message], String] =
    Impl.parseFile(inputPath).map(AstPrinter.print(_))

  private object Impl {
    def parseFile(inputPath: String): Either[Seq[Message], Ast.Block] =
      FileIo.readFile(inputPath).flatMap { input =>
        Parser.parseString(inputPath, input)
      }

    def analyzeAndEmit(programBlock: Ast.Block): Either[Seq[Message], (String, Seq[Message])] = {
      val analyzed = Analyzer.analyzeProgram(programBlock)

      analyzed.program.toRight(analyzed.messages).map { program =>
        val jsAst = JsEmitter.emitModule(program)
        (JsPrinter.printModule(jsAst), analyzed.messages)
      }
    }
  }

}
