package clara

import ai.x.safe._

sealed trait Pos {
  def format: String
}
case class SourcePos(sourceInfo: SourceInfo, fromIndex: Int, untilIndex: Option[Int]) extends Pos {
  def format = {
    val (fromLine, fromCol) = sourceInfo.lineCol(fromIndex)
    val from = safe"${fromLine.toString}:${fromCol.toString}"
    val until = untilIndex.map { untilIndex =>
      val (untilLine, untilCol) = sourceInfo.lineCol(untilIndex)

      safe"(-${untilLine.toString}:${untilCol.toString})"
    }.getOrElse("")

    safe"${sourceInfo.name}:$from$until"
  }
}
case object NoPos extends Pos {
  val format = "unknown position"
}
