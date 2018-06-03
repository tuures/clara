package clara

object Clara {

  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import fastparse.core.Parsed
    Parser.parseProgramBlock(input) match {
      case Parsed.Success(block, index) =>
        import sext._
        println(block.treeString)
        println()

        val blockWithPrelude = {
          import Ast._
          Block(Prelude.Prelude ++ Seq(block))
        }
        Analyzer.analyze(blockWithPrelude) match {
          case Right(t) => println(t.signature(Analyzer.Env.empty))
          case Left(errors) => println(errors.map(e => s"Semantic error: $e").mkString("\n"))
        }
        println()

        println(JsEmitter.emitString(blockWithPrelude))
        println()

      case Parsed.Failure(p, index, extra) =>
        println("Parse error: " + extra.traced.trace)
    }
  }
}
