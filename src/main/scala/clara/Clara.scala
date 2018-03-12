package clara

object Clara {

  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import fastparse.core.Parsed
    Parser.parse(input) match {
      case Parsed.Success(v, index) =>
        import sext._
        println(v.treeString)
        println()

        val env = Stdlib.baseEnv
        Analyzer.analyze(env)(v) match {
          case Right(t) => println(t.toSource(env))
          case Left(errors) => println(errors.map(e => s"Semantic error: $e").mkString("\n"))
        }
        println()

        println(JsEmitter.emitString(v))
        println()

      case Parsed.Failure(p, index, extra) =>
        println("Parse error: " + extra.traced.trace)
    }
  }
}
