package clara.ast

import org.scalatest.FunSuite

class SourceInfoSpec extends FunSuite {
  def testSourceInfoFromString(testDesc: String)(
    content: String
  )(
    expectedLength: Int, expectedNewlineIndices: Seq[Int]
  ) = test(s"SourceInfo.fromString: $testDesc") {
    val s = SourceInfo.fromString("foo.file", content)

    assert(s.length === expectedLength)
    assert(s.newlineIndices == expectedNewlineIndices)
  }

  testSourceInfoFromString("file with no content")(
    ""
  )(
    0,
    Nil
  )

  testSourceInfoFromString("file with one line")(
    "first\n"
  )(
    6,
    Seq(5)
  )

  testSourceInfoFromString("file with one line and no newline at the end")(
    "first"
  )(
    5,
    Nil
  )

  testSourceInfoFromString("file with multiple lines")(
    "first\n\nthird\nfourth\n"
  )(
    20,
    Seq(5, 6, 12, 19)
  )

  testSourceInfoFromString("file with multiple lines and no newline at the end")(
    "first\n\nthird"
  )(
    12,
    Seq(5, 6)
  )

  test("LineCol from SourceInfo") {
    val s = SourceInfo.fromString("foo.file", "first\n\nthird\nfourth\nlast")

    val expectedLineColsByIndex = Seq(
      (0, (0, 0)),
      (1, (0, 1)),
      (4, (0, 4)),
      (5, (0, 5)),
      (6, (1, 0)),
      (7, (2, 0)),
      (8, (2, 1)),
      (11, (2, 4)),
      (12, (2, 5)),
      (13, (3, 0)),
      (23, (4, 3)),
    )
    val indices = expectedLineColsByIndex.map(_._1)
    val expectedLineCols = expectedLineColsByIndex.map(_._2).map { case (line, col) =>
      LineCol(line, col)
    }

    val lineColsFromSourceInfo = indices.map(s.lineCol(_))

    assertThrows[IllegalArgumentException](s.lineCol(-1))
    assert(lineColsFromSourceInfo === expectedLineCols)
    assertThrows[IllegalArgumentException](s.lineCol(24))
  }
}
