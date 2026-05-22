package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends SwiftSrc2CpgSuite {

  "`if-else` statements connect then and else branches via TRUE_BODY/FALSE_BODY edges" in {
    val cpg = code("""
      |func method(x: Int) {
      |  if x > 0 {
      |    sinkThen(x)
      |    sinkThen2(x)
      |  } else {
      |    sinkElse(x)
      |    sinkElse2(x)
      |  }
      |}
      |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifNode: ControlStructure) =>
        ifNode.condition.code.l shouldBe List("x > 0")
        ifNode.trueBodyOut.astChildren.code.l shouldBe List("sinkThen(x)", "sinkThen2(x)")
        ifNode.falseBodyOut.astChildren.code.l shouldBe List("sinkElse(x)", "sinkElse2(x)")
    }
  }

  "`if-elseif-else` statements connect then and else branches via TRUE_BODY/FALSE_BODY edges" in {
    val cpg = code("""
      |func method(c: Int) {
      |  if c > 10 {
      |    sinkA(c)
      |    sinkA2(c)
      |  } else if c < 10 {
      |    sinkB(c)
      |    sinkB2(c)
      |  } else {
      |    sinkC(c)
      |    sinkC2(c)
      |  }
      |}
      |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifOne: ControlStructure, ifTwo: ControlStructure) =>
        ifOne.condition.code.l shouldBe List("c > 10")
        ifOne.trueBodyOut.astChildren.code.l shouldBe List("sinkA(c)", "sinkA2(c)")
        ifOne.falseBodyOut.l shouldBe List(ifTwo)

        ifTwo.condition.code.l shouldBe List("c < 10")
        ifTwo.trueBodyOut.astChildren.code.l shouldBe List("sinkB(c)", "sinkB2(c)")
        ifTwo.falseBodyOut.astChildren.code.l shouldBe List("sinkC(c)", "sinkC2(c)")
    }
  }

  "`if` without `else` has no FALSE_BODY edge" in {
    val cpg = code("""
      |func method(x: Int) {
      |  if x > 0 {
      |    sink(x)
      |    sink2(x)
      |  }
      |}
      |""".stripMargin)

    inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l) {
      case List(ifNode: ControlStructure) =>
        ifNode.trueBodyOut.astChildren.code.l shouldBe List("sink(x)", "sink2(x)")
        ifNode.falseBodyOut.l shouldBe List.empty
    }
  }

  "`repeat-while` (do-while) statements connect the body via DO_BODY edge" in {
    val cpg = code("""
      |func method(c: Int) {
      |  repeat {
      |    sink(c)
      |    sink2(c)
      |  } while c < 10
      |}
      |""".stripMargin)

    inside(cpg.method.name("method").doBlock.l) { case List(doNode: ControlStructure) =>
      doNode.condition.code.l shouldBe List("c < 10")
      doNode.doBodyOut.astChildren.code.l shouldBe List("sink(c)", "sink2(c)")
    }
  }

  "`do-catch` statements connect try and catch bodies via explicit edges" in {
    val cpg = code("""
      |func method() {
      |  do {
      |    sink()
      |    sink2()
      |  } catch {
      |    sinkCatch()
      |    sinkCatch2()
      |  }
      |}
      |""".stripMargin)

    inside(cpg.controlStructure.isTry.l) { case List(tryNode: ControlStructure) =>
      tryNode.tryBodyOut.astChildren.code.l shouldBe List("sink()", "sink2()")
      inside(tryNode.catchBodyOut.l) { case List(catchNode: ControlStructure) =>
        catchNode.controlStructureType shouldBe ControlStructureTypes.CATCH
        catchNode.astChildren.isBlock.astChildren.code.l shouldBe List("sinkCatch()", "sinkCatch2()")
      }
      tryNode.finallyBodyOut.l shouldBe List.empty
    }
  }

}
