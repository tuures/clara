package clara

import ai.x.safe._

sealed trait Pos {
  def humanFormat: String
}
case class SourcePos(sourceInfo: SourceInfo, fromIndex: Int, untilIndex: Option[Int]) extends Pos {
  def humanFormat = {
    val fromLineCol = sourceInfo.lineCol(fromIndex)
    val from = fromLineCol.humanFormat
    val until = untilIndex.map { untilIndex =>
      val lineCol = sourceInfo.lineCol(untilIndex - 1)
      // use inclusive range format for humans, thus - 1
      safe"(-${if (lineCol.line === fromLineCol.line) lineCol.humanFormatCol else lineCol.humanFormat})"
    }.getOrElse("")

    safe"${sourceInfo.name}:$from$until"
  }
}
case object NoPos extends Pos {
  val humanFormat = "unknown position"
}
