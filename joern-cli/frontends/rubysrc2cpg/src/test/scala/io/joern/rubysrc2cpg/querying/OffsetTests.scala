package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class OffsetTests extends RubyCode2CpgFixture(disableFileContent = false) {

  "ASCII-only code" should {
    val cpg = code("""
        |def f
        | x = 42
        |end
        |""".stripMargin)

    "have correct offsets on the literal" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "2-byte UTF-8 char (é) shifts subsequent offsets" should {
    // "é" is 2 UTF-8 bytes but 1 UTF-16 code unit
    val cpg = code("""café = 1
        |x = 2
        |""".stripMargin)

    "have correct offset for literal after multi-byte char" in {
      inside(cpg.literal.code("2").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "2"
      }
    }
  }

  "3-byte UTF-8 chars (中文) shift subsequent offsets" should {
    // "中" is 3 UTF-8 bytes but 1 UTF-16 code unit.
    val cpg = code("""s = "中文"
        |x = 42
        |""".stripMargin)

    "have correct offset for literal after CJK chars" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "4-byte UTF-8 char (🎉) shifts subsequent offsets" should {
    // "🎉" is 4 UTF-8 bytes but 2 UTF-16 code units (drift = +2).
    val cpg = code("""# 🎉
        |x = 42
        |""".stripMargin)

    "have correct offset for literal after emoji" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "ERB files" should {
    val cpg = code(
      """<h1>Hello, <%= ENV['APP_NAME'] %>!</h1>
        |<% if ENV['SHOW_VERSION'] == 'true' %>
        |  <p>Version: <%= ENV['APP_VERSION'] %></p>
        |<% end %>
        |""".stripMargin,
      "test.erb"
    )

    "not have offsets" in {
      cpg.literal.l should not be empty
      cpg.literal.l.foreach { lit =>
        lit.offset shouldBe None
        lit.offsetEnd shouldBe None
      }
    }
  }

}
