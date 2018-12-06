package clara

import ai.x.safe._

object ClaraCli {

  case class Options(
    val printAst: Boolean = false,
    val inputPath: String = "-",
    val outputPath: Option[String] = None
  )

  object Options {
    def parse(args: List[String]) = {
      def p(as: List[String], o: Options = Options()): Option[Options] = as match {
        case "-a" :: rest => p(rest, o.copy(printAst = true))
        case "-o" :: str :: rest => p(rest, o.copy(outputPath = Some(str)))
        case str :: Nil => Some(o.copy(inputPath = str))
        case _ => None
      }

      p(args)
    }
  }

  val usage = s"""
  |Usage: [options] <input file>
  |Options:
  |  -a\tprint parsed AST
  |  -o\toutput path
  """.stripMargin

  def run(options: Options) = FileIo.readFile(options.inputPath).flatMap { input =>
    Parser(options.inputPath, input).parseAsProgramBlock
  }.flatMap { block =>
    if (options.printAst) {
      import sext._
      println(block.treeString)
      println()
    }

    val blockWithPrelude = Prelude.prependTo(block)

    // options.outputPath.foreach { outputPath =>
    //   FileIo.writeFile(outputPath, JsEmitter.emitString(blockWithPrelude)) match {
    //     case Left(errors) => println(errors.map(_.humanFormat).safeMkString("\n"))
    //     case Right(()) => ()
    //   }
    // }

    Analyzer.analyze(blockWithPrelude)
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


  object FileIo {
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    def readFile(path: String): Either[Seq[Error], String] = {
      try {
        Right(new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8))
      } catch {
        case e: Exception => Left(Seq(GeneralError(safe"Could not read file `$path`: ${e.toString()}")))
      }
    }

    def writeFile(path: String, content: String): Either[Seq[Error], Unit] = {
      try {
        Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8))

        Right(())
      } catch {
        case e: Exception => Left(Seq(GeneralError(safe"Could not write file `$path`: ${e.toString()}")))
      }
    }
  }
}
