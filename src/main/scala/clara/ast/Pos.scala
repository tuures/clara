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
  // Note: empty program block (evaluates to unit type) requires both
  // fromIndex and untilIndex to be 0 while sourceInfo.length is also 0
  // otherwise fromIndex should be less than sourceInfo.length
  require(fromIndex >= 0 && fromIndex <= sourceInfo.length, s"fromIndex ${fromIndex} out of range")
  untilIndex.foreach { i =>
    require(i >= fromIndex && i <= sourceInfo.length, s"untilIndex ${i} out of range")
  }

  // TODO don't crash if all three are 0 (empty program block) but still produce a reasonable human format
  def humanFormat = {
    val startLineCol = sourceInfo.lineCol(fromIndex)
    val start = startLineCol.humanFormat
    val end = untilIndex.flatMap { untilIndex =>
      // use inclusive range format for humans, thus - 1
      val endLineCol = sourceInfo.lineCol(untilIndex - 1)

      if (endLineCol.line === startLineCol.line) {
        if (endLineCol.col === startLineCol.col) {
          None
        } else {
          Some(endLineCol.humanFormatCol)
        }
      } else {
        Some(endLineCol.humanFormat)
      }
    }

    val ndash = "\u2013"

    safe"${sourceInfo.name}:$start${end.fold("")(end => safe"($ndash$end)")}"
  }

  override def toString() = safe"SourcePos($humanFormat)"
}

case object NoPos extends Pos {
  val humanFormat = "unknown position"
}
