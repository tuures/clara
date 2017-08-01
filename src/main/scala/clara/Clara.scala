package clara

object Clara {

  val baseEnv = {
    import collection.immutable.HashMap
    import Analyzer._

    Env.empty.copy(types = HashMap(
      "()" -> new Type0(),
      "Int" -> new Type0(),
      "String" -> new Type0()//,
      // "Tuple2" -> Type(),
      // "Function" -> Type()
    ))
  }

  def main(args:Array[String]): Unit = {
    val input = args.head
    println(input)

    import fastparse.core.Parsed
    Parser.parse(input) match {
      case Parsed.Success(v, index) =>
        import sext._
        println(v.treeString)

        Analyzer.analyze(baseEnv)(v) match {
          case Right(t) => println(t.toSource(baseEnv))
          case Left(errors) => println(errors.map(e => s"Semantic error: $e").mkString("\n"))
        }
        println()
      case Parsed.Failure(p, index, extra) =>
        println("Parse error: " + extra.traced.trace)
    }
  }
}
