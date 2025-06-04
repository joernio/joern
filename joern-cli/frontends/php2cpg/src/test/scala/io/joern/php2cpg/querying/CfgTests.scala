package io.joern.php2cpg.querying

import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, JumpTarget}
import io.shiftleft.semanticcpg.language.*

class CfgTests extends PhpCode2CpgFixture {
  "the CFG for match constructs" when {
    "the match does not have a default case" should {
      val cpg = code("""<?php
          |function foo() {
          |  match (cond()) {
          |    'X' => body1(),
          |    'Y', 'Z' => body2(),
          |  };
          |  sink();
          |}
          |""".stripMargin)
      "find that the jump targets and sink call are CFG successors of cond call" in {
        inside(cpg.call.name("cond").cfgNext.l) {
          case List(xTarget: JumpTarget, yTarget: JumpTarget, zTarget: JumpTarget, sink: Call) =>
            xTarget.name shouldBe "case \"X\""
            yTarget.name shouldBe "case \"Y\""
            zTarget.name shouldBe "case \"Z\""
            sink.code shouldBe "sink()"
        }
      }

      "find that the sink call is the CFG successor of the body1 call" in {
        inside(cpg.call.name("body1").cfgNext.l) { case List(sinkCall: Call) =>
          sinkCall.code shouldBe "sink()"
        }
      }

      "find that the sink call is the CFG successor of the body2 call" in {
        inside(cpg.call.name("body2").cfgNext.l) { case List(sinkCall: Call) =>
          sinkCall.code shouldBe "sink()"
        }
      }
    }

    "the match has a default case" should {
      val cpg = code("""<?php
          |function foo() {
          |  match (cond()) {
          |    'X' => body1(),
          |    'Y', 'Z' => body2(),
          |    default => body3(),
          |  };
          |  sink();
          |}
          |""".stripMargin)

      "only the jump targets are CFG successors of the cond call" in {
        inside(cpg.call.name("cond").cfgNext.l) {
          case List(xTarget: JumpTarget, yTarget: JumpTarget, zTarget: JumpTarget, defaultTarget: JumpTarget) =>
            xTarget.name shouldBe "case \"X\""
            yTarget.name shouldBe "case \"Y\""
            zTarget.name shouldBe "case \"Z\""
            defaultTarget.name shouldBe "default"
        }
      }

      "find that the sink call is the CFG successor of the body1 call" in {
        inside(cpg.call.name("body1").cfgNext.l) { case List(sinkCall: Call) =>
          sinkCall.code shouldBe "sink()"
        }
      }

      "find that the sink call is the CFG successor of the body2 call" in {
        inside(cpg.call.name("body2").cfgNext.l) { case List(sinkCall: Call) =>
          sinkCall.code shouldBe "sink()"
        }
      }

      "find that the sink call is the CFG successor of the body3 call" in {
        inside(cpg.call.name("body3").cfgNext.l) { case List(sinkCall: Call) =>
          sinkCall.code shouldBe "sink()"
        }
      }
    }
  }

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

  "the CFG for foreach constructs" should {
    val cpg = code("""<?php
        |function foo() {
        |  pre();
        |  for ($i = 0; $i < 10; $i++) {
        |    sink($i);
        |  }
        |  post();
        |}
        |""".stripMargin)

    "find that the sink call is controlled by the comparison" in {
      cpg.call.name("sink").controlledBy.collectAll[Call].code.toSet should contain("$i < 10")
    }

    "find that the initializer dominates the sink call" in {
      cpg.call.name("sink").dominatedBy.collectAll[Call].code.toSet should contain("$i = 0")
    }

    "find that the sink call does not dominate the post call" in {
      cpg.call.name("sink").dominates.collectAll[Call].name.toSet should not contain "post"
    }
  }

  "the CFG for try constructs" should {
    val cpg = code("""<?php
        |function foo() {
        |  call1();
        |  try {
        |    call2();
        |  } catch (A $a) {
        |    call3();
        |  } catch (B $b) {
        |    call4();
        |  } finally {
        |    call5();
        |  }
        |  call6();
        |}
        |""".stripMargin)

    "find that call1 dominates all other calls" in {
      cpg.call.name("call1").dominates.collectAll[Call].name.toSet shouldBe (2 to 6).map(num => s"call$num").toSet
    }

    "find that call6 is post dominated by the try and finally calls" in {
      cpg.call.name("call1").postDominatedBy.collectAll[Call].name.toSet shouldBe Set("call2", "call5", "call6")
    }

    "find that call3 is controlled by call2" in {
      cpg.call.name("call3").controlledBy.collectAll[Call].name.toSet should contain("call2")
    }

    "find that call6 is not controlled by call2" in {
      cpg.call.name("call6").controlledBy.collectAll[Call].name.toSet should not contain ("call2")
    }
  }

  "the CFG for gotos" should {
    val cpg = code("""<?php
		 |function foo() {
		 |  if (cond()) {
         |    sink1();
         |    goto SKIP;
		 |  } else {
		 |    SKIP:
		 |    sink2();
		 |  }
		 |}
		 |""".stripMargin)
    "find that sink1 is post dominated by sink2" in {
      cpg.call.name("sink1").postDominatedBy.collectAll[Call].name.toSet should contain("sink2")
    }

    "find that sink2 is not controlled by cond" in {
      cpg.call.name("sink2").controlledBy.collectAll[Call].name.toSet should not contain "cond"
    }
  }
}
