package clara.ast

import clara.testutil.BaseSpec

class PosSpec extends BaseSpec {

  val sourceInfo = SourceInfo.fromString("foo.file", "abc\ndef\nghi")

  def testSourcePosHumanFormat(sp: SourcePos)(expected: String) = {
    import clara.util.Safe.SafeStringContext

    val indices = safe"${sp.fromIndex.toString()} ${sp.untilIndex.toString()}"
    test(safe"SourcePos.humanFormat $indices -> $expected") {
      assert(sp.humanFormat === expected)
    }
  }

  testSourcePosHumanFormat(SourcePos(sourceInfo, 0, None))("foo.file:1:1")
  testSourcePosHumanFormat(SourcePos(sourceInfo, 0, Some(2)))("foo.file:1:1(–2)")
  testSourcePosHumanFormat(SourcePos(sourceInfo, 3, None))("foo.file:1:4")
  testSourcePosHumanFormat(SourcePos(sourceInfo, 3, Some(4)))("foo.file:1:4")
  testSourcePosHumanFormat(SourcePos(sourceInfo, 4, Some(5)))("foo.file:2:1")
  testSourcePosHumanFormat(SourcePos(sourceInfo, 0, Some(6)))("foo.file:1:1(–2:2)")
}
