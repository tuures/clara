package clara

import clara.analyzer.Analyzer
import clara.ast.AstPrinter
import clara.parser.Parser
import clara.util.FileIo

import ai.x.safe._

object ClaraCli {

  case class Options(
    val printAst: Boolean = false,
    val inputPath: String = "-",
    val outputPath: Option[String] = None
  )

  val PrintAst = "-p"
  val OutputPath = "-o"

  def parseCmdLineOptions(args: List[String]) = {
    def p(as: List[String], o: Options = Options()): Either[String, Options] = as match {
      case `PrintAst` :: rest => p(rest, o.copy(printAst = true))
      case `OutputPath` :: str :: rest => p(rest, o.copy(outputPath = Some(str)))
      case str :: Nil => Right(o.copy(inputPath = str))
      case list => Left(s"Unknown option: ${list.safeMkString(" ")}")
    }

    p(args)
  }

  val usage = s"""
  |Usage: [options] <input file>
  |Options:
  |  ${PrintAst}\tprint parsed AST
  |  ${OutputPath}\toutput path
  """.stripMargin

  def run(options: Options) = FileIo.readFile(options.inputPath).flatMap { input =>
    Parser(options.inputPath, input).parseAsProgramBlock
  }.flatMap { programBlock =>
    if (options.printAst) {
      println(AstPrinter.print(programBlock))
      println()
    }

    // val blockWithPrelude = Prelude.prependTo(block)

    // options.outputPath.foreach { outputPath =>
    //   FileIo.writeFile(outputPath, JsEmitter.emitString(blockWithPrelude)) match {
    //     case Left(errors) => println(errors.map(_.humanFormat).safeMkString("\n"))
    //     case Right(()) => ()
    //   }
    // }

    // Analyzer.analyze(blockWithPrelude)
    Analyzer.analyzeProgramBlock(programBlock)
  } match {
    // case Right(resultType) => println(resultType.signature(Analyzer.Env.empty))
    case Right(_) => println("")
    case Left(errors) =>
      println(errors.map(_.humanFormat).safeMkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    parseCmdLineOptions(args.toList) match {
      case Right(options) => run(options)
      case Left(errorMessage) => {
        println(usage)
        println(errorMessage)
      }
    }
  }

}
