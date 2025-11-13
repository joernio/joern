package io.joern.php2cpg.querying

import io.joern.php2cpg.Config
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
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
        |print("hello");
        |""".stripMargin,
      "test.php",
      StandardCharsets.ISO_8859_1
    ).withConfig(contentEnabled)

    "have correct offsets" in {
      inside(cpg.call.name("print").l) {
        case (printCall: Call) :: Nil =>
          printCall.offset shouldBe Some(15)
          printCall.offsetEnd shouldBe Some(29)
        case xs =>
          fail(s"Expected a single 'print' call, instead got ${xs.count}")
      }
    }

    "contain the correct symbol for the print call" in {
      inside(cpg.call.name("print").l) {
        case (printCall: Call) :: Nil =>
          printCall.location.symbol shouldBe """print("hello")"""
        case xs =>
          fail(s"Expected a single 'print' call, instead got ${xs.count}")
      }
    }
  }

  "a file encoded with UTF-8" should {
    val cpg = code(
      """<?php
        |
        |// ääú
        |
        |print("hello");
        |""".stripMargin,
      "test.php",
      StandardCharsets.UTF_8
    ).withConfig(contentEnabled)

    "have correct offsets" in {
      inside(cpg.call.name("print").l) {
        case (printCall: Call) :: Nil =>
          printCall.offset shouldBe Some(15)
          printCall.offsetEnd shouldBe Some(29)
        case xs =>
          fail(s"Expected a single 'print' call, instead got ${xs.count}")
      }
    }

    "contain the correct symbol for the print call" in {
      inside(cpg.call.name("print").l) {
        case (printCall: Call) :: Nil =>
          printCall.location.symbol shouldBe """print("hello")"""
        case xs =>
          fail(s"Expected a single 'print' call, instead got ${xs.count}")
      }
    }
  }
}
