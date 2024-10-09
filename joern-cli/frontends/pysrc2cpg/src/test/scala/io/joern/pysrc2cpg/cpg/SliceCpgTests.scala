package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class SliceCpgTests extends PySrc2CpgFixture() {
  "slice" should {
    "have correct AST case 1" in {
      val cpg = code("""
          |x[1:2:1]
          |""".stripMargin)

      val sliceOperator = cpg.call.name("<operator>.slice").head
      sliceOperator.code shouldBe "x[1:2:1]"
      sliceOperator.argument(1).code shouldBe "x"
      sliceOperator.argument(2).code shouldBe "1"
      sliceOperator.argument(3).code shouldBe "2"
      sliceOperator.argument(4).code shouldBe "1"
    }

    "have correct AST case 2" in {
      val cpg = code("""
          |x[::]
          |""".stripMargin)

      val sliceOperator = cpg.call.name("<operator>.slice").head
      sliceOperator.code shouldBe "x[::]"
      sliceOperator.argument(1).code shouldBe "x"
      sliceOperator.argument(2).code shouldBe "None"
      sliceOperator.argument(3).code shouldBe "None"
      sliceOperator.argument(4).code shouldBe "None"
    }

    "have correct AST case 3" in {
      val cpg = code("""
          |x[::][1:2]
          |""".stripMargin)

      val outerOperator = cpg.call.name("<operator>.slice").codeExact("x[::][1:2]").head
      val innerOperator = outerOperator.argument.argumentIndex(1).isCall.head

      innerOperator.code shouldBe "x[::]"
      innerOperator.argument(1).code shouldBe "x"
      innerOperator.argument(2).code shouldBe "None"
      innerOperator.argument(3).code shouldBe "None"
      innerOperator.argument(4).code shouldBe "None"

      outerOperator.argument(2).code shouldBe "1"
      outerOperator.argument(3).code shouldBe "2"
      outerOperator.argument(4).code shouldBe "None"
    }
  }

}
