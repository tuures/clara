package clara.ast

import ai.x.safe._

// TODO unit tests for this file, lot's of + - 1 index stuff

// zero-indexed line and col
case class LineCol(line: Int, col: Int) {
  // for humans use 1-indexed presentation
  private def humanFormatIndex(i: Int): String = (i + 1).toString
  def humanFormatLine = humanFormatIndex(line)
  def humanFormatCol = humanFormatIndex(col)
  def humanFormat = safe"$humanFormatLine:$humanFormatCol"
}

case class SourceInfo(name: String, newlineIndices: Vector[Int], length: Int) {
  def lineCol(index: Int): LineCol = {
    require(index >= 0 && index < length, "index out of range")

    // TODO this could be optimised using a binary search for precessor
    val lineIndex = newlineIndices.lastIndexWhere(index > _)
    if (lineIndex === -1) {
      LineCol(0, index)
    } else {
      val newlineIndex = newlineIndices(lineIndex)
      LineCol(lineIndex + 1, index - (newlineIndex + 1))
    }
  }
}

object SourceInfo {
  def fromString(name: String, input: String) =
    SourceInfo(name, "\n".r.findAllMatchIn(input).map(_.start).toVector, input.length)
}
