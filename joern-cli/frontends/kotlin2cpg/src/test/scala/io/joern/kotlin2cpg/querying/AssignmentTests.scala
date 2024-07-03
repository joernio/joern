package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class AssignmentTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple assignments" should {
    val cpg = code("""
        |fun main(args : Array<String>) {
        |  val x: Int = 5
        |  x += 1
        |  x -= 1
        |  x *= 1
        |  x /= 1
        |  x %= 1
        |}
        |""".stripMargin)

    "should contain a call node for the `assignment` operator" in {
      cpg.call(Operators.assignment).size should not be 0
    }

    "should contain a call node for the `assignmentPlus` operator" in {
      cpg.call(Operators.assignmentPlus).size should not be 0
    }

    "should contain a call node for the `assignmentMinus` operator" in {
      cpg.call(Operators.assignmentMinus).size should not be 0
    }

    "should contain a call node for the `assignmentMultiplication` operator" in {
      cpg.call(Operators.assignmentMultiplication).size should not be 0
    }

    "should contain a call node for the `assignmentDivision` operator" in {
      cpg.call(Operators.assignmentDivision).size should not be 0
    }

    "should contain a call node for the `assignmentModulo` operator" in {
      cpg.call(Operators.assignmentModulo).size should not be 0
    }

    "should contain a call node for `assignment` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignment).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(3)
      p.code shouldBe "val x: Int = 5"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a call node for `assignmentPlus` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignmentPlus).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(4)
      p.code shouldBe "x += 1"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a call node for `assignmentMinus` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignmentMinus).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(5)
      p.code shouldBe "x -= 1"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a call node for `assignmentMultiplication` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignmentMultiplication).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(6)
      p.code shouldBe "x *= 1"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a call node for `assignmentDivision` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignmentDivision).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(7)
      p.code shouldBe "x /= 1"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }

    "should contain a call node for `assignmentModulo` op with correct props set" in {
      val List(p) = cpg.call.methodFullName(Operators.assignmentModulo).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(8)
      p.code shouldBe "x %= 1"
      p.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
