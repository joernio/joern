package io.joern.php2cpg.querying

import io.joern.php2cpg.Config
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, ControlStructure}
import io.shiftleft.semanticcpg.language.*

import java.nio.charset.StandardCharsets

class OffsetTests extends PhpCode2CpgFixture {
  private val contentEnabled = Config().withDisableFileContent(false)

  "a file encoded with ISO8859-1" should {
    val cpg = code(
      """<?php
        |
        |// ääú
        |
        |foreach ($arr as $key => $val) {};
        |""".stripMargin,
      "test.php",
      StandardCharsets.ISO_8859_1
    ).withConfig(contentEnabled)

    "have correct offsets" in {
      inside(cpg.controlStructure.l) { case (forEach: ControlStructure) :: Nil =>
        forEach.code shouldBe "foreach ($arr as $key => $val)"
        forEach.offset shouldBe Some(15)
        forEach.offsetEnd shouldBe Some(48)
      }
    }

    "contain the correct symbol for the print call" in {
      inside(cpg.controlStructure.l) { case (forEach: ControlStructure) :: Nil =>
        forEach.location.symbol shouldBe """foreach ($arr as $key => $val) {}"""
      }
    }
  }

  "a file encoded with UTF-8" should {
    val cpg = code(
      """<?php
        |
        |// ääú
        |
        |foreach ($arr as $key => $val) {};
        |""".stripMargin,
      "test.php",
      StandardCharsets.UTF_8
    ).withConfig(contentEnabled)

    "have correct offsets" in {
      inside(cpg.controlStructure.l) { case (forEach: ControlStructure) :: Nil =>
        forEach.code shouldBe "foreach ($arr as $key => $val)"
        forEach.offset shouldBe Some(15)
        forEach.offsetEnd shouldBe Some(48)
      }
    }

    "contain the correct symbol for the print call" in {
      inside(cpg.controlStructure.l) { case (forEach: ControlStructure) :: Nil =>
        forEach.location.symbol shouldBe """foreach ($arr as $key => $val) {}"""
      }
    }
  }

  "UTF-8 with emoji should have correct offsets" should {
    // "🎉" is 4 UTF-8 bytes but 2 UTF-16 code units.
    // php2cpg correctly converts byte offsets to char offsets,
    // so the literal after the emoji should have the right offset.
    val cpg = code(
      """<?php
        |$a = "🎉";
        |$b = 42;
        |""".stripMargin,
      "test.php",
      StandardCharsets.UTF_8
    ).withConfig(contentEnabled)

    "have correct offset for literal after emoji" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "UTF-8 with CJK characters should have correct offsets" should {
    // "中文" — each char is 3 UTF-8 bytes but 1 UTF-16 code unit.
    val cpg = code(
      """<?php
        |$s = "中文";
        |$x = 99;
        |""".stripMargin,
      "test.php",
      StandardCharsets.UTF_8
    ).withConfig(contentEnabled)

    "have correct offset for literal after CJK chars" in {
      inside(cpg.literal.code("99").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "99"
      }
    }
  }

  "ISO-8859-1 with accented characters should have correct offsets" should {
    // "äöü" are each a single byte in ISO-8859-1 (0xE4, 0xF6, 0xFC), unlike UTF-8 where
    // they would be 2 bytes each. So byte offset == UTF-16 offset here; this test guards
    // against a conversion that assumes UTF-8 and would incorrectly shift these offsets.
    val cpg = code(
      """<?php
        |$s = "äöü";
        |$x = 77;
        |""".stripMargin,
      "test.php",
      StandardCharsets.ISO_8859_1
    ).withConfig(contentEnabled)

    "have correct offset for literal after accented chars" in {
      inside(cpg.literal.code("77").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "77"
      }
    }
  }
}
