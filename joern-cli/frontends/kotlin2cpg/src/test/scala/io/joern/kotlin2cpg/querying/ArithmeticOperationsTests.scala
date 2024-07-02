package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ArithmeticOperationsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {

  "CPG for code with simple arithmetic operations" should {

    val cpg = code("""
        |fun main(args : Array<String>) {
        |  println(1 + 2)
        |  println(1 - 2)
        |  println(1 * 2)
        |  println(1 / 2)
        |}
        |""".stripMargin)

    "should contain a CALL node for the addition op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.addition).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(3)
      p.code shouldBe "1 + 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a CALL node for the subtraction op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.subtraction).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(4)
      p.code shouldBe "1 - 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a CALL node for the multiplication op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.multiplication).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(5)
      p.code shouldBe "1 * 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a CALL node for the division op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.division).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(6)
      p.code shouldBe "1 / 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
