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
}
