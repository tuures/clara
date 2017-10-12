package clara

object Clara {

  val baseEnv = {
    import collection.immutable.HashMap
    import Analyzer._

    val intType = new TypeCon(HashMap.empty[String, TypeInst])
    Env.empty.copy(types = HashMap(
      "()" -> new TypeCon(HashMap.empty[String, TypeInst]),
      "Int" -> intType,
      "String" -> new TypeCon(HashMap("length" -> intType.inst().getOrElse(???)))
      // "Tuple" -> new TypeCon(),
      // "Function" -> new TypeCon()
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
        println()

        Analyzer.analyze(baseEnv)(v) match {
          case Right(t) => println(t.toSource(baseEnv))
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
