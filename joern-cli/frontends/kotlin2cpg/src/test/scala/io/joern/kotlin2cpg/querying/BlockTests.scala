package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.nodes.Block
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BlockTests extends AnyFreeSpec with Matchers {

  "CPG for code with a simple function definition" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun add1mul(x: Int, y: Int): Int {
        |  return (x + 1) * y
        |}
        |""".stripMargin)

    "should contain a BLOCK node with the correct props set" in {
      val List(b) = cpg.method.block.take(1).l
      b.lineNumber shouldBe Some(4)
      b.columnNumber shouldBe Some(33)
      b.typeFullName shouldBe "java.lang.Void"
      b.code should not be ""
    }
  }
}
