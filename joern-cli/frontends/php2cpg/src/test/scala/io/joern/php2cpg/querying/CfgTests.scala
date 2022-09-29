package io.joern.php2cpg.querying

import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class CfgTests extends PhpCode2CpgFixture {

  "the CFG for if constructs" should {
    val cpg = code("""<?php
		 |function foo($x, $y) {
		 |  if ($x < 10) {
		 |    sink1();
		 |  } elseif ($y > 5) {
		 |    sink2();
		 |  } else {
		 |    sink3();
		 |  }
		 |  echo "foo";
		 |}
		 |""".stripMargin)

    "find that sink1 is control dependent on the if condition" in {
      inside(cpg.call.name("sink1").controlledBy.isCall.code.l) { case List(controllerCode) =>
        controllerCode shouldBe "$x < 10"
      }
    }

    "find that sink2 is control dependent on the if and elseif conditions" in {
      inside(cpg.call.name("sink2").controlledBy.isCall.code.sorted.toList) { case List(xCode, yCode) =>
        xCode shouldBe "$x < 10"
        yCode shouldBe "$y > 5"
      }
    }
  }
}
