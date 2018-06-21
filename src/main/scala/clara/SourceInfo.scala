package clara

case class SourceInfo(name: String, lineIndices: Seq[Int]) {
  def lineCol(index: Int) = lineIndices.view.zipWithIndex.
    find(index >= _._1).map { case (lineStart, lineIndex) =>
      (lineIndex, index - lineStart)
    }.getOrElse((0, index))
}

object SourceInfo {
  def fromString(name: String, input: String) =
    SourceInfo(name, "\n".r.findAllMatchIn(input).map(_.start).toList)
}
