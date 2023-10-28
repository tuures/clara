package clara

import clara.analyzer.Analyzer
import clara.ast.AstPrinter
import clara.jsemitter.JsEmitter
import clara.jsemitter.impl.JsPrinter
import clara.parser.Parser
import clara.util.{FileIo, Message}

import clara.util.Safe._

object ClaraCli {

  case class Options(
    val printAst: Boolean = false,
    val inputPath: String = "-",
    val outputPath: Option[String] = None
  )

  val PrintAst = "-p"
  val OutputPath = "-o"

  def parseCmdLineOptions(args: List[String]) = {
    def unexpected(s: String) = Left(s"Unexpected command line argument: $s")
    def p(as: List[String], o: Options = Options()): Either[String, Options] = as match {
      case `PrintAst` :: rest => p(rest, o.copy(printAst = true))
      case `OutputPath` :: str :: rest => p(rest, o.copy(outputPath = Some(str)))
      case str :: rest if (o.inputPath === "-") => p(rest, o.copy(inputPath = str))
      case Nil => Right(o)
      case rest => unexpected(rest.safeString(" "))
    }

    p(args)
  }

  val usage = safe"""
  |Usage: [options] <input file>
  |Options:
  |  ${PrintAst}\tprint parsed AST
  |  ${OutputPath}\toutput path
  """.stripMargin

  def printMessages(prefix: String, messages: Seq[Message]): Unit =
    messages.map(_.humanFormat.replaceAll("(^|\n)", "$1" + prefix)).foreach(println)

  def run(options: Options): Unit = (FileIo.readFile(options.inputPath).flatMap { input =>
    Parser.parseString(options.inputPath, input)
  }.flatMap { programBlock =>
    if (options.printAst) {
      println(AstPrinter.print(programBlock))
      println()
    }

    val (asgOrErrors, warnings) = Analyzer.analyzeProgramBlock(programBlock)

    printMessages("WARN: ", warnings)

    asgOrErrors
  }.map { asg =>
    JsEmitter.emitProgram(asg)
  }.map { jsAst =>
    JsPrinter.emitString(jsAst)
  }.flatMap { jsSource =>
    options.outputPath match {
      case Some(outputPath) => FileIo.writeFile(outputPath, jsSource)
      case None => Right(println(jsSource))
    }
  }) match {
    case Right(()) => ()
    case Left(errors) => printMessages("ERROR: ", errors)
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
