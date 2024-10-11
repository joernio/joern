package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*

class PatternMatchingTests extends PySrc2CpgFixture() {
  "pattern matching" should {
    "be correct" in {
      val cpg = code("""match [1, 2]:
          |  case [a, b]:
          |    print(1)
          |  case _:
          |    print(2)
          |""".stripMargin)
      val switch    = cpg.controlStructure.head
      val condition = switch.astChildren.order(1).head
      condition.code shouldBe "[1, 2]"
      condition.lineNumber shouldBe Some(1)

      val case1 = switch.astChildren.order(2).head
      case1.label shouldBe NodeTypes.BLOCK
      case1.code shouldBe "print(1)"

      val case2 = switch.astChildren.order(3).head
      case2.label shouldBe NodeTypes.BLOCK
      case2.code shouldBe "print(2)"
    }
  }

}
