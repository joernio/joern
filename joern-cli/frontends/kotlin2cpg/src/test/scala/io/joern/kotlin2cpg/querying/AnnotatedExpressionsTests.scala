package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class AnnotatedExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with two identical calls, one annotated and one not" should {
    lazy val cpg = code("""
        |package mypkg
        |
        |fun main() {
        |    @Suppress("DEPRECATION")
        |    println("AMESSAGE")
        |
        |    println("AMESSAGE")
        |}
        |""".stripMargin)

    "should contain two CALL nodes with identical CODE,MFN & DISPATCH_TYPE props" in {
      val List(c1, c2) = cpg.call.code("println.*").l
      c1.code shouldBe c2.code
      c1.methodFullName shouldBe c2.methodFullName
      c1.dispatchType shouldBe c2.dispatchType
    }
  }
}
