package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends JsSrc2CpgSuite {

  "`if-else` statements should connect then and else branches via TRUE_BODY/FALSE_BODY edges" in {
    val cpg = code("""
        |function method(x) {
        |  if (x > 0) {
        |    y = 0;
        |  } else {
        |    y = 1;
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifNode: ControlStructure) =>
        ifNode.condition.code.l shouldBe List("x > 0")
        ifNode.trueBodyOut.astChildren.code.l shouldBe List("y = 0")
        inside(ifNode.falseBodyOut.l) { case List(elseStructure: ControlStructure) =>
          elseStructure.controlStructureType shouldBe ControlStructureTypes.ELSE
          elseStructure.astChildren.isBlock.astChildren.code.l shouldBe List("y = 1")
        }
    }
  }

  "`if-elseif-else` statements should connect then and else branches via TRUE_BODY/FALSE_BODY edges" in {
    val cpg = code("""
        |function method(c) {
        |  if (c > 10) {
        |    c -= 10;
        |  } else if (c < 10) {
        |    c += 10;
        |  } else {
        |    c = 10;
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifOne: ControlStructure, ifTwo: ControlStructure) =>
        ifOne.condition.code.l shouldBe List("c > 10")
        ifOne.trueBodyOut.astChildren.code.l shouldBe List("c -= 10")

        inside(ifOne.falseBodyOut.l) { case List(elseOne: ControlStructure) =>
          elseOne.controlStructureType shouldBe ControlStructureTypes.ELSE
          elseOne.astChildren.l shouldBe List(ifTwo)
        }

        ifTwo.condition.code.l shouldBe List("c < 10")
        ifTwo.trueBodyOut.astChildren.code.l shouldBe List("c += 10")

        inside(ifTwo.falseBodyOut.l) { case List(elseTwo: ControlStructure) =>
          elseTwo.controlStructureType shouldBe ControlStructureTypes.ELSE
          elseTwo.astChildren.isBlock.astChildren.code.l shouldBe List("c = 10")
        }
    }
  }

  "`if` without `else` has no FALSE_BODY edge" in {
    val cpg = code("""
        |function method(x) {
        |  if (x > 0) {
        |    y = 0;
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifNode: ControlStructure) =>
        ifNode.trueBodyOut.astChildren.code.l shouldBe List("y = 0")
        ifNode.falseBodyOut.l shouldBe List.empty
    }
  }

  "`do-while` statements connect the body via DO_BODY edge" in {
    val cpg = code("""
        |function method(c) {
        |  do {
        |    c += 1;
        |  } while (c < 10);
        |}
        |""".stripMargin)

    inside(cpg.method.name("method").doBlock.l) { case List(doNode: ControlStructure) =>
      doNode.condition.code.l shouldBe List("c < 10")
      doNode.doBodyOut.astChildren.code.l shouldBe List("c += 1")
    }
  }

  "`for-loop` statements connect init, update and body via dedicated edges" in {
    val cpg = code("""
        |function method(c) {
        |  for (var i = 0; i < c; i++) {
        |    sink(i);
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.FOR).l) {
      case List(forNode: ControlStructure) =>
        forNode.forInitOut.code.l shouldBe List("var i = 0")
        forNode.condition.code.l shouldBe List("i < c")
        forNode.forUpdateOut.code.l shouldBe List("i++")
        forNode.forBodyOut.astChildren.code.l shouldBe List("sink(i)")
    }
  }

  "`for-loop` statements without init, test or update emit empty block FOR_INIT/FOR_UPDATE edges" in {
    val cpg = code("for(;;){ sink(); }")

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.FOR).l) {
      case List(forNode: ControlStructure) =>
        forNode.forInitOut.isBlock.astChildren.l shouldBe List.empty
        forNode.condition.isBlock.astChildren.l shouldBe List.empty
        forNode.forUpdateOut.isBlock.astChildren.l shouldBe List.empty
        forNode.forBodyOut.astChildren.code.l shouldBe List("sink()")
    }
  }

  "`try-catch-finally` statements connect try, catch and finally bodies via explicit edges" in {
    val cpg = code("""
        |function method(c) {
        |  try {
        |    sink(c);
        |  } catch (e) {
        |    sinkCatch(e);
        |  } finally {
        |    sinkFinally();
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.isTry.l) { case List(tryNode: ControlStructure) =>
      tryNode.tryBodyOut.astChildren.code.l shouldBe List("sink(c)")
      inside(tryNode.catchBodyOut.l) { case List(catchNode: ControlStructure) =>
        catchNode.controlStructureType shouldBe ControlStructureTypes.CATCH
        catchNode.astChildren.isBlock.astChildren.code.l shouldBe List("e", "sinkCatch(e)")
      }
      inside(tryNode.finallyBodyOut.l) { case List(finallyNode: ControlStructure) =>
        finallyNode.controlStructureType shouldBe ControlStructureTypes.FINALLY
        finallyNode.astChildren.isBlock.astChildren.code.l shouldBe List("sinkFinally()")
      }
    }
  }

  "`try-finally` statement without catch emits no CATCH_BODY edge" in {
    val cpg = code("""
        |function method() {
        |  try {
        |    sink();
        |  } finally {
        |    sinkFinally();
        |  }
        |}
        |""".stripMargin)

    inside(cpg.controlStructure.isTry.l) { case List(tryNode: ControlStructure) =>
      tryNode.tryBodyOut.astChildren.code.l shouldBe List("sink()")
      tryNode.catchBodyOut.l shouldBe List.empty
      tryNode.finallyBodyOut.astChildren.isBlock.astChildren.code.l shouldBe List("sinkFinally()")
    }
  }

}
