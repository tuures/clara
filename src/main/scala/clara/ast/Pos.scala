package clara.ast

import clara.util.Safe._

sealed trait Pos {
  def humanFormat: String
  def join(later: Pos): Pos = (this, later) match {
    case (pos1: SourcePos, pos2: SourcePos) => SourcePos(pos1.sourceInfo, pos1.fromIndex, pos2.untilIndex)
    case _ => NoPos
  }
}
case class SourcePos(sourceInfo: SourceInfo, fromIndex: Int, untilIndex: Option[Int]) extends Pos {
  def humanFormat = {
    val fromLineCol = sourceInfo.lineCol(fromIndex)
    val from = fromLineCol.humanFormat
    val until = untilIndex.map { untilIndex =>
      // use inclusive range format for humans, thus - 1
      val lineCol = sourceInfo.lineCol(untilIndex - 1)
      val ndash = "\u2013"
      safe"($ndash${if (lineCol.line === fromLineCol.line) lineCol.humanFormatCol else lineCol.humanFormat})"
    }.getOrElse("")

    safe"${sourceInfo.name}:$from$until"
  }

  override def toString() = safe"SourcePos($humanFormat)"
}
case object NoPos extends Pos {
  val humanFormat = "unknown position"
}
