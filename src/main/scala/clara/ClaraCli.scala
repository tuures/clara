package clara

import clara.util.{FileIo, Message}

import clara.util.Safe._

object ClaraCli {

  case class Options(
    val inputPath: String,
    val outputPath: Option[String],
  )

  val OutputPath = "-o"

  def parseCmdLineOptions(args: List[String]) = {
    def unexpected(s: String) = Left(s"Unexpected command line argument: $s")
    def p(as: List[String], inputPath: Option[String], outputPath: Option[String]): Either[String, Options] = as match {
      case `OutputPath` :: str :: rest => p(rest, inputPath, Some(str))
      case str :: rest if inputPath.isEmpty => p(rest, Some(str), outputPath)
      case Nil => inputPath.toRight("Missing input file").map(Options(_, outputPath))
      case rest => unexpected(rest.safeString(" "))
    }

    p(args, None, None)
  }

  val usage = safe"""
  |Usage: [options] <input file>
  |Options:
  |  ${OutputPath}\toutput path
  """.stripMargin

  def printMessages(prefix: String, messages: Seq[Message]): Unit =
    messages.map(_.humanFormat.replaceAll("(^|\n)", "$1" + prefix)).foreach(println)

  def run(options: Options): Unit =
    Compiler.compileFile(options.inputPath).flatMap { case (jsSource, warnings) =>
      printMessages("WARN: ", warnings)
      options.outputPath match {
        case Some(outputPath) => FileIo.writeFile(outputPath, jsSource)
        case None => Right(println(jsSource))
      }
    } match {
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
