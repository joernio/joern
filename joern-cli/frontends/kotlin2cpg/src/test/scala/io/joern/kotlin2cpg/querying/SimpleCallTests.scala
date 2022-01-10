package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SimpleCallTests extends AnyFreeSpec with Matchers {
  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |fun main(args : Array<String>) {
      |  println(1)
      |}
      |""".stripMargin)

  "should contain the correct number of CALL nodes" in {
    cpg.call.size shouldBe 1
  }

  "arguments should have the correct values" in {
    val List(x) = cpg.call.argument.isLiteral.l
    x.argumentIndex shouldBe 1
    x.code shouldBe "1"
    x.order shouldBe 1
    x.typeFullName shouldBe "kotlin.Int"
    x.lineNumber shouldBe Some(2)
    x.columnNumber shouldBe Some(10)
  }
}
