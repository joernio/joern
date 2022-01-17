package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticOperationsTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple arithmetic operations" - {

    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun main(args : Array<String>) {
        |  println(1 + 2)
        |  println(1 - 2)
        |  println(1 * 2)
        |  println(1 / 2)
        |}
        |""".stripMargin)

    "should contain a call node for addition op with correct fields" in {
      val List(p) = cpg.call.methodFullName(Operators.addition).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(2)
      p.code shouldBe "1 + 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
    }

    "should contain a call node for subtraction op with correct fields" in {
      val List(p) = cpg.call.methodFullName(Operators.subtraction).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(3)
      p.code shouldBe "1 - 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
    }

    "should contain a call node for multiplication op with correct fields" in {
      val List(p) = cpg.call.methodFullName(Operators.multiplication).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(4)
      p.code shouldBe "1 * 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
    }

    "should contain a call node for division op with correct fields" in {
      val List(p) = cpg.call.methodFullName(Operators.division).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(5)
      p.code shouldBe "1 / 2"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
    }
  }
}
