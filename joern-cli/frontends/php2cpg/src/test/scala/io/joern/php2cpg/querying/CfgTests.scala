package io.joern.php2cpg.querying

import io.joern.php2cpg.parser.Domain.PhpBuiltins
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

    "find that sink3 is control dependent on the if and elseif conditions" in {
      inside(cpg.call.name("sink3").controlledBy.isCall.code.sorted.toList) { case List(xCode, yCode) =>
        xCode shouldBe "$x < 10"
        yCode shouldBe "$y > 5"
      }
    }

    "find that the if controls sink1" in {
      inside(cpg.controlStructure.condition.codeExact("$x < 10").l) { case List(condition) =>
        condition.controls.isCall.name("sink1").l.size shouldBe 1
      }
    }

    "find that echo post dominates all" in {
      cpg.call("echo").postDominates.size shouldBe 11
    }

    "find that the method does not post dominate anything" in {
      inside(cpg.method("foo").l) { case List(method) =>
        method.postDominates.size shouldBe 0
      }
    }
  }

  "the CFG for while constructs" should {
    val cpg = code("""<?php
		 |function foo($x) {
     |  while($x < 10) {
     |    sink();
     |  }
		 |}
		 |""".stripMargin)

    "find that the sink is control dependent on the condition" in {
      inside(cpg.call("sink").controlledBy.isCall.l) { case List(condition) =>
        condition.code shouldBe "$x < 10"
      }
    }

    "find that the sink call does not dominate anything" in {
      cpg.call.name("sink").dominates.size shouldBe 0
    }
  }

  "the CFG for switch constructs" should {
    val cpg = code("""<?php
		 |function foo($x) {
     |  switch ($x < 10) {
     |    case 0:
     |      sink1();
     |    case 1:
     |      sink2();
     |      break;
     |    case 3:
     |    case 4:
     |      sink3();
     |    default:
     |      sink4();
     |  }
		 |}
		 |""".stripMargin)

    "find that all the sinks are control dependent on condition" in {
      (1 to 4).foreach { num =>
        inside(cpg.call.nameExact(s"sink$num").controlledBy.isCall.code.l) { case List(code) =>
          code shouldBe "$x < 10"
        }
      }
    }

    "find that sink2 post dominates sink1" in {
      cpg.call("sink2").postDominates.isCall.name.toSet should contain("sink1")
    }

    "find that sink3 does not post dominate sink2" in {
      cpg.call("sink3").postDominates.isCall.name.toSet should not contain "sink2"
    }
  }
}
