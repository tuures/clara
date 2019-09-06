package clara.util

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import ai.x.safe._

object FileIo {

  def readFile(path: String): Either[Seq[Message], String] = {
    try {
      Right(new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8))
    } catch {
      case e: Exception => Left(Seq(GeneralMessage(safe"Could not read file `$path`: ${e.toString()}")))
    }
  }

  def writeFile(path: String, content: String): Either[Seq[Message], Unit] = {
    try {
      Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8))

      Right(())
    } catch {
      case e: Exception => Left(Seq(GeneralMessage(safe"Could not write file `$path`: ${e.toString()}")))
    }
  }

}
