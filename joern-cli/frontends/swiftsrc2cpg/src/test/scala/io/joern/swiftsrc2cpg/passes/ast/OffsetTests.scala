package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class OffsetTests extends SwiftSrc2CpgSuite {

  private val contentEnabled = Config().withDisableFileContent(false)

  "ASCII-only code" should {
    val cpg = code("""
        |func f() {
        | let x = 42
        |}
        |""".stripMargin).withConfig(contentEnabled)

    "have correct offsets on the literal" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        lit.offset shouldBe defined
        lit.offsetEnd shouldBe defined
      }
    }
  }

  "2-byte UTF-8 char (é) shifts subsequent offsets" should {
    val cpg = code(
      """func f() {
        | let café = 1
        | let next = 2
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after multi-byte char" in {
      inside(cpg.literal.code("2").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "2"
      }
    }
  }

  "3-byte UTF-8 chars (中文) shift subsequent offsets" should {
    val cpg = code(
      """func f() {
        | let s = "中文"
        | let x = 42
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after CJK chars" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "4-byte UTF-8 char (🎉) in comment shifts subsequent offsets" should {
    val cpg = code(
      """// 🎉
        |func f() {
        | let x = 1
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after emoji" in {
      inside(cpg.literal.code("1").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "1"
      }
    }
  }

}
