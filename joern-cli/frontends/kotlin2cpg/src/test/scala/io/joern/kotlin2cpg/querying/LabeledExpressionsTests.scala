package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class LabeledExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with simple call to `println` prefixed by a label" should {
    val cpg = code("""
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
