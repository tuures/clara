package clara

import clara.testutil.BaseSpec
import clara.util.Message

import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import scala.sys.process._

class E2eSpec extends BaseSpec {
  val fixturesDir = Paths.get("src/test/resources/e2e")

  def compile(claraFile: String): Either[Seq[Message], (String, Seq[Message])] =
    Compiler.compileFile(fixturesDir.resolve(claraFile).toString)

  def runJs(js: String): (Int, String, String) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    val exitCode = Seq("node", "-e", js) ! ProcessLogger(
      line => { stdout.append(line).append("\n"); () },
      line => { stderr.append(line).append("\n"); () },
    )

    (exitCode, stdout.toString.stripSuffix("\n"), stderr.toString.stripSuffix("\n"))
  }

  def readFile(path: Path): Option[String] =
    if (Files.exists(path)) Some(new String(Files.readAllBytes(path), StandardCharsets.UTF_8).stripSuffix("\n"))
    else None

  def formatMessages(messages: Seq[Message]): String =
    messages.map(_.humanFormat).mkString("\n")

  // Discover and register all .clara files in the fixtures directory
  if (Files.exists(fixturesDir)) {
    Files.list(fixturesDir).toArray.map(_.asInstanceOf[Path]).filter(_.toString.endsWith(".clara")).sorted.foreach { claraPath =>
      val fileName = claraPath.getFileName.toString
      val testName = fileName.replaceFirst("\\.clara$", "")
      val outputPath = fixturesDir.resolve(testName + ".output")
      val messagesPath = fixturesDir.resolve(testName + ".messages")
      val expectedOutput = readFile(outputPath)
      val expectedMessages = readFile(messagesPath)

      if (expectedOutput.isDefined || expectedMessages.isDefined) {
        test(s"e2e: $testName") {
          compile(fileName) match {
            case Left(errors) =>
              if (expectedOutput.isDefined) fail(s"Compilation failed:\n${formatMessages(errors)}")
              expectedMessages.foreach { expected =>
                assert(formatMessages(errors) === expected, "Compiler error mismatch")
              }
            case Right((js, warnings)) =>
              expectedMessages.foreach { expected =>
                assert(formatMessages(warnings) === expected, "Compiler warning mismatch")
              }
              expectedOutput.foreach { expected =>
                val (exitCode, stdout, stderr) = runJs(js)
                assert(exitCode === 0, s"Node.js exited with code $exitCode\nstderr: $stderr\njs:\n$js")
                assert(stdout === expected, s"Output mismatch.\nExpected:\n$expected\nActual:\n$stdout\njs:\n$js")
              }
          }
        }
      }
    }
  }

}
