package clara

import ai.x.safe._

object ClaraCli {

  case class Options(
    val printAst: Boolean,
    val inputPath: String
  )

  object Options {
    def defaults = Options(
      printAst = false,
      inputPath = ""
    )
    def parse(args: List[String]) = {
      def p(as: List[String], o: Options = defaults): Option[Options] = as match {
        case "-a" :: rest => p(rest, o.copy(printAst = true))
        case str :: Nil => Some(o.copy(inputPath = str))
        case _ => None
      }

      p(args)
    }
  }

  val usage = """
  | Usage: [options] <input file>
  | Options:
  |   -a\tprint parsed AST
  """.stripMargin

  def readInput(path: String): Either[Seq[Error], String] = {
    import scala.io.Source

    try {
      val s = Source.fromFile(path, "UTF-8")
      try {
        Right(s.getLines.mkString("\n"))
      } finally s.close()
    } catch {
      case e: Exception => Left(Seq(GeneralError(safe"Could not read input file `$path`: ${e.toString()}")))
    }
  }

  def run(options: Options) = readInput(options.inputPath).flatMap { input =>
    Parser(options.inputPath, input).parseAsProgramBlock
  }.flatMap { block =>
    if (options.printAst) {
      import sext._
      println(block.treeString)
      println()
    }

    // println(JsEmitter.emitString(blockWithPrelude))
    // println()

    Analyzer.analyze(Prelude.prependTo(block))
  } match {
    case Right(resultType) => println(resultType.signature(Analyzer.Env.empty))
    case Left(errors) =>
      println(errors.map(_.humanFormat).safeMkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    Options.parse(args.toList).map { options =>
      run(options)
    }.getOrElse {
      println(usage)
    }
  }
}
