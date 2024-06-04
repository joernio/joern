package io.joern.php2cpg.passes

import io.joern.php2cpg.testfixtures.PhpFrontend
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.AlwaysEdge
import io.joern.x2cpg.testfixtures.{CfgTestCpg, CfgTestFixture}
import io.shiftleft.codepropertygraph.generated.Cpg

class PhpCfgTestCpg extends CfgTestCpg with PhpFrontend

// The tests in this class are not supposed to test all of the CPG generated
// since we have plenty of over tests for that. Instead we are only testing
// for the edge resulting from leveled break and continue statements as they
// only appear in PHP.
class CfgCreationPassTests extends CfgTestFixture(() => new PhpCfgTestCpg) {
  override def code(code: String): PhpCfgTestCpg = {
    super.code(s"""<?php
         |function func() {
         |  $code;
         |}
         |""".stripMargin)
  }

  "Cfg for nested while loop" should {
    "be correct for break with level 1" in {
      implicit val cpg: Cpg = code("""
          |while ($i < 1) {
          |  while ($j < 1) {
          |    break 1;
          |  }
          |}
          |""".stripMargin)
      succOf("break(1)") shouldBe expected(("$i", AlwaysEdge))
    }

    "be correct for break with level 2" in {
      implicit val cpg: Cpg = code("""
          |while ($i < 1) {
          |  while ($j < 1) {
          |    break 2;
          |  }
          |}
          |""".stripMargin)
      succOf("break(2)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for continue with level 1" in {
      implicit val cpg: Cpg = code("""
          |while ($i < 1) {
          |  while ($j < 1) {
          |    continue 1;
          |  }
          |}
          |""".stripMargin)
      succOf("continue(1)") shouldBe expected(("$j", AlwaysEdge))
    }

    "be correct for continue with level 2" in {
      implicit val cpg: Cpg = code("""
          |while ($i < 1) {
          |  while ($j < 1) {
          |    continue 2;
          |  }
          |}
          |""".stripMargin)
      succOf("continue(2)") shouldBe expected(("$i", AlwaysEdge))
    }
  }

  "Cfg for nested do loop" should {
    "be correct for break with level 1" in {
      implicit val cpg: Cpg = code("""
          |do {
          |  do {
          |    break 1;
          |  } while ($j < 1);
          |} while ($i < 1)
          |""".stripMargin)
      succOf("break(1)") shouldBe expected(("$i", AlwaysEdge))
    }

    "be correct for break with level 2" in {
      implicit val cpg: Cpg = code("""
          |do {
          |  do {
          |    break 2;
          |  } while ($j < 1);
          |} while ($i < 1)
          |""".stripMargin)
      succOf("break(2)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for continue with level 1" in {
      implicit val cpg: Cpg = code("""
          |do {
          |  do {
          |    continue 1;
          |  } while ($j < 1);
          |} while ($i < 1)
          |""".stripMargin)
      succOf("continue(1)") shouldBe expected(("$j", AlwaysEdge))
    }

    "be correct for continue with level 2" in {
      implicit val cpg: Cpg = code("""
          |do {
          |  do {
          |    continue 2;
          |  } while ($j < 1);
          |} while ($i < 1)
          |""".stripMargin)
      succOf("continue(2)") shouldBe expected(("$i", AlwaysEdge))
    }
  }

  "Cfg for nested for loop" should {
    "be correct for break with level 1" in {
      implicit val cpg: Cpg = code("""
          |for ($i = 0; $i < 1; $i++) {
          |  for ($j = 0; $j < 1; $j++) {
          |    break 1;
          |  }
          |}
          |""".stripMargin)
      succOf("break(1)") shouldBe expected(("$i", AlwaysEdge))
    }

    "be correct for break with level 2" in {
      implicit val cpg: Cpg = code("""
          |for ($i = 0; $i < 1; $i++) {
          |  for ($j = 0; $j < 1; $j++) {
          |    break 2;
          |  }
          |}
          |""".stripMargin)
      succOf("break(2)") shouldBe expected(("RET", AlwaysEdge))
    }

    "be correct for continue with level 1" in {
      implicit val cpg: Cpg = code("""
          |for ($i = 0; $i < 1; $i++) {
          |  for ($j = 0; $j < 1; $j++) {
          |    continue 1;
          |  }
          |}
          |""".stripMargin)
      succOf("continue(1)") shouldBe expected(("$j", AlwaysEdge))
    }

    "be correct for continue with level 2" in {
      implicit val cpg: Cpg = code("""
          |for ($i = 0; $i < 1; $i++) {
          |  for ($j = 0; $j < 1; $j++) {
          |    continue 2;
          |  }
          |}
          |""".stripMargin)
      succOf("continue(2)") shouldBe expected(("$i", AlwaysEdge))
    }
  }

  "Cfg for nested switch" should {
    "be correct for break with level 1" in {
      implicit val cpg: Cpg = code("""
          |switch ($i) {
          |  case 0:
          |    switch ($j) {
          |      case 0:
          |        break 1;
          |    }
          |    $k;
          |}
          |""".stripMargin)
      succOf("break(1)") shouldBe expected(("$k", AlwaysEdge))
    }

    "be correct for break with level 2" in {
      implicit val cpg: Cpg = code("""
          |switch ($i) {
          |  case 0:
          |    switch ($j) {
          |      case 0:
          |        break 2;
          |    }
          |    $k;
          |}
          |""".stripMargin)
      succOf("break(2)") shouldBe expected(("RET", AlwaysEdge))
    }
  }
}
