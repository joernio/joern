package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class PatternTests extends AstSwiftSrc2CpgSuite {

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

    "testNonBinding3" ignore {
      val cpg = code("if case let (y[0], z) = x {}")
      ???
    }

    "testNonBinding4" ignore {
      val cpg = code("""
        |switch x {
        |  case let (y[0], z):
        |    z
        |}
        |""".stripMargin)
      ???
    }

    "testNonBinding5" ignore {
      val cpg = code("if case let y[z] = x {}")
      ???
    }

    "testNonBinding6" ignore {
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
      ???
    }

  }

}
