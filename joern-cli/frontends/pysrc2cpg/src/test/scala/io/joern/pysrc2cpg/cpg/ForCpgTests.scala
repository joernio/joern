package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers

class ForCpgTests extends PySrc2CpgFixture with Matchers {
  "for statement lowering" should {
    val cpg = code("""for x in y:
        |  z
        |else:
        |  w""".stripMargin)

    "emit while-based control structure with explicit true/false body edges" in {
      // For loops use a while-loop based lowering, so get the while loop node
      val List(whileNode) = cpg.controlStructure.isWhile.l
      whileNode.trueBodyOut.isBlock.size shouldBe 1
      whileNode.falseBodyOut.code.l shouldBe List("w")
    }
  }
}
