package io.joern.pysrc2cpg.cpg

import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture

class ReturnCpgTests extends PySrc2CpgFixture with Matchers {
  "void return" should {
    val cpg = code("""def func(a, b):
        |  return
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.head
      returnNode.code shouldBe "return"
      returnNode.lineNumber shouldBe Some(2)
    }
  }

  "value return" should {
    val cpg = code("""def func(a, b):
        |  return a
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.head
      returnNode.code shouldBe "return a"
      returnNode.lineNumber shouldBe Some(2)
    }

    "test return node ast children" in {
      cpg.ret.astChildren.order(1).isIdentifier.head.code shouldBe "a"
    }
  }

  "value return with lowering" should {
    val cpg = code("""def func(a, b):
                                                |  return {a:1}
                                                |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.head
      returnNode.code shouldBe "return {a:1}"
      returnNode.lineNumber shouldBe Some(2)
    }

    "test return node ast children" in {
      cpg.ret.astChildren.order(1).head.code shouldBe
        "tmp0 = {}\ntmp0[a] = 1\ntmp0"
    }
  }

}
