package clara

import ai.x.safe._

object Clara {

  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    Parser("stdin", input).parseAsProgramBlock match {
      case Right(block) =>
        import sext._
        println(block.treeString)
        println()

        val blockWithPrelude = {
          import Ast._
          Block(Prelude.Prelude ++ Seq(block))
        }
        Analyzer.analyze(blockWithPrelude) match {
          case Right(t) => println(t.signature(Analyzer.Env.empty))
          case Left(errors) => println(errors.map(_.format).safeMkString("\n"))
        }
        println()

        // println(JsEmitter.emitString(blockWithPrelude))
        // println()

      case Left(errors) =>
        println(errors.map(_.format).safeMkString("\n"))
    }
  }
}
