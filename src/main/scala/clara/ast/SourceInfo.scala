package clara.ast

import ai.x.safe._

// zero-indexed line and col
case class LineCol(line: Int, col: Int) {
  // for humans use 1-indexed presentation
  private def humanFormatIndex(i: Int): String = (i + 1).toString
  def humanFormatLine = humanFormatIndex(line)
  def humanFormatCol = humanFormatIndex(col)
  def humanFormat = safe"$humanFormatLine:$humanFormatCol"
}

case class SourceInfo(name: String, lineIndices: Seq[Int], length: Int) {
  def lineCol(index: Int): LineCol = {
    assert(index < length, "index out of range")

    lineIndices.view.zipWithIndex.
      find(index >= _._1).
      map { case (lineStart, lineIndex) =>
        LineCol(lineIndex, index - lineStart)
      }.getOrElse(LineCol(0, index))
  }
}

object SourceInfo {
  def fromString(name: String, input: String) =
    SourceInfo(name, "\n".r.findAllMatchIn(input).map(_.start).toList, input.length)
}
