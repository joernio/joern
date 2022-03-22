package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LabeledExpressionsTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with simple call to `println` prefixed by a label" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    aLabel@ println("AMESSAGE")
        |}
        |""".stripMargin)

    "should contain a CALL node for the call to `println`" in {
      val List(c) = cpg.call.code(".*println.*").l
      c.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      c.code shouldBe "println(\"AMESSAGE\")"
    }
  }
}
