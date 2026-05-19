package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class PatternTests extends SwiftSrc2CpgSuite {

  "PatternTests" should {

    "testNonBinding1" ignore {
      val cpg = code("if case let E<Int>.e(y) = x {}")
      ???
    }

    "testNonBinding2" ignore {
      val cpg = code("""
        |switch e {
        |  case let E<Int>.e(y):
        |    y
        |}
        |""".stripMargin)
      ???
    }

    // testNonBinding3-6 contain invalid Swift patterns (subscript inside binding). The parser
    // recovers and we still emit the surrounding IF/SWITCH control structure. Assert on that.

    "testNonBinding3" in {
      val cpg          = code("if case let (y[0], z) = x {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if case let")
    }

    "testNonBinding4" in {
      val cpg = code("""
        |switch x {
        |  case let (y[0], z):
        |    z
        |}
        |""".stripMargin)
      val List(switchNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchNode.code should startWith("switch x")
    }

    "testNonBinding5" in {
      val cpg          = code("if case let y[z] = x {}")
      val List(ifNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      ifNode.code should startWith("if case let")
    }

    "testNonBinding6" in {
      val cpg = code("""
        |switch 0 {
        |  case let y[z]:
        |    z
        |   case y[z]:
        |     0
        |   default:
        |     0
        |}
        |""".stripMargin)
      val List(switchNode) = cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l
      switchNode.code should startWith("switch 0")
    }

  }

}
