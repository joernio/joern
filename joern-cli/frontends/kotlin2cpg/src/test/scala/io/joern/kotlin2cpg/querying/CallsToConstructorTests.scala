package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, Local}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallsToConstructorTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with call to constructor of Java stdlib object inside declaration" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import java.io.File
        |
        |fun main() {
        |    val f = File("/tmp/myfile.txt")
        |    f.writeText("Hello, world!")
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(local) = cpg.local.nameExact("f").l
      local.typeFullName shouldBe "java.io.File"

      val List(assignmentCall) = cpg.call.methodFullNameExact(Operators.assignment).take(1).l
      assignmentCall.signature shouldBe ""

      val List(assignmentLhs: Identifier, assignmentRhs: Block) = assignmentCall.argument.l
      assignmentLhs.code shouldBe "f"
      assignmentLhs.name shouldBe "f"
      assignmentLhs.lineNumber shouldBe Some(6)
      assignmentLhs.columnNumber shouldBe Some(8)
      local.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "java.io.File"
      assignmentRhs.code shouldBe ""

      val List(firstBlockChild: Local) = assignmentRhs.astChildren.take(1).l
      firstBlockChild.name shouldBe "tmp_1"
      firstBlockChild.typeFullName shouldBe "java.io.File"

      val List(secondBlockChild: Call) = assignmentRhs.astChildren.drop(1).take(1).l
      val allocAssignment = secondBlockChild
      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = allocAssignment.argument.l
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "java.io.File"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(thirdBlockChild: Call) = assignmentRhs.astChildren.drop(2).take(1).l
      val initCall = thirdBlockChild
      initCall.code shouldBe "File(\"/tmp/myfile.txt\")"
      initCall.signature shouldBe "void(kotlin.String)"
      initCall.methodFullName shouldBe "java.io.File.<init>:void(kotlin.String)"
      initCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(initCallLhs: Identifier, initCallRhs: Literal) = initCall.argument.l
      initCallLhs.code shouldBe "tmp_1"
      initCallLhs.typeFullName shouldBe "java.io.File"
      initCallLhs.argumentIndex shouldBe 0
      firstBlockChild.referencingIdentifiers.id.l.contains(initCallLhs.id) shouldBe true

      initCallRhs.code shouldBe "\"/tmp/myfile.txt\""
      initCallRhs.typeFullName shouldBe "kotlin.String"
      initCallRhs.argumentIndex shouldBe 1

      val List(fourthBlockChild: Identifier) = assignmentRhs.astChildren.drop(3).take(1).l
      fourthBlockChild.code shouldBe "tmp_1"
      fourthBlockChild.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(fourthBlockChild.id) shouldBe true
    }
  }

  "CPG for code with call to constructor of Java stdlib object inside QE" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import java.io.File
        |
        |fun main() {
        |     File("/tmp/myfile.txt").writeText("Hello, world!")
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(qeCall) = cpg.call.methodFullName(".*writeText.*").l

      val List(assignmentLhs: Block, assignmentRhs: Literal) = qeCall.argument.l
      val loweredBlock = assignmentLhs
      loweredBlock.typeFullName shouldBe "java.io.File"
      loweredBlock.code shouldBe ""

      val List(firstBlockChild: Local) = loweredBlock.astChildren.take(1).l
      firstBlockChild.name shouldBe "tmp_1"
      firstBlockChild.typeFullName shouldBe "java.io.File"

      val List(secondBlockChild: Call) = loweredBlock.astChildren.drop(1).take(1).l
      val allocAssignment = secondBlockChild
      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = allocAssignment.argument.l
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "java.io.File"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(thirdBlockChild: Call) = loweredBlock.astChildren.drop(2).take(1).l
      val initCall = thirdBlockChild
      initCall.code shouldBe "File(\"/tmp/myfile.txt\")"
      initCall.signature shouldBe "void(kotlin.String)"
      initCall.methodFullName shouldBe "java.io.File.<init>:void(kotlin.String)"
      initCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(initCallLhs: Identifier, initCallRhs: Literal) = initCall.argument.l
      initCallLhs.code shouldBe "tmp_1"
      initCallLhs.typeFullName shouldBe "java.io.File"
      initCallLhs.argumentIndex shouldBe 0
      firstBlockChild.referencingIdentifiers.id.l.contains(initCallLhs.id) shouldBe true

      initCallRhs.code shouldBe "\"/tmp/myfile.txt\""
      initCallRhs.typeFullName shouldBe "kotlin.String"
      initCallRhs.argumentIndex shouldBe 1

      val List(fourthBlockChild: Identifier) = loweredBlock.astChildren.drop(3).take(1).l
      fourthBlockChild.code shouldBe "tmp_1"
      fourthBlockChild.typeFullName shouldBe "java.io.File"
      firstBlockChild.referencingIdentifiers.id.l.contains(fourthBlockChild.id) shouldBe true
    }
  }

  "CPG for code with call to simple constructor of user-defined class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(val x: String) {
        |    fun printX() {
        |        println(x)
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass("AMESSAGE")
        |    a.printX()
        |}
        |""".stripMargin)

    "should contain a correct lowered representation" in {
      val List(local) = cpg.local.nameExact("a").l
      local.typeFullName shouldBe "mypkg.AClass"

      val List(assignmentCall) = cpg.call.methodFullNameExact(Operators.assignment).take(1).l
      assignmentCall.signature shouldBe ""

      val List(assignmentLhs: Identifier, assignmentRhs: Block) = assignmentCall.argument.l
      assignmentLhs.code shouldBe "a"
      assignmentLhs.name shouldBe "a"
      assignmentLhs.lineNumber shouldBe Some(10)
      assignmentLhs.columnNumber shouldBe Some(8)
      local.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true
      assignmentRhs.typeFullName shouldBe "mypkg.AClass"
      assignmentRhs.code shouldBe ""

      val List(firstBlockChild: Local) = assignmentRhs.astChildren.take(1).l
      firstBlockChild.name shouldBe "tmp_1"
      firstBlockChild.typeFullName shouldBe "mypkg.AClass"

      val List(secondBlockChild: Call) = assignmentRhs.astChildren.drop(1).take(1).l
      val allocAssignment = secondBlockChild
      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = allocAssignment.argument.l
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "mypkg.AClass"
      firstBlockChild.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "mypkg.AClass"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(thirdBlockChild: Call) = assignmentRhs.astChildren.drop(2).take(1).l
      val initCall = thirdBlockChild
      initCall.code shouldBe "AClass(\"AMESSAGE\")"
      initCall.signature shouldBe "void(kotlin.String)"
      initCall.methodFullName shouldBe "mypkg.AClass.<init>:void(kotlin.String)"
      initCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(initCallLhs: Identifier, initCallRhs: Literal) = initCall.argument.l
      initCallLhs.code shouldBe "tmp_1"
      initCallLhs.typeFullName shouldBe "mypkg.AClass"
      initCallLhs.argumentIndex shouldBe 0
      firstBlockChild.referencingIdentifiers.id.l.contains(initCallLhs.id) shouldBe true

      initCallRhs.code shouldBe "\"AMESSAGE\""
      initCallRhs.typeFullName shouldBe "kotlin.String"
      initCallRhs.argumentIndex shouldBe 1

      val List(fourthBlockChild: Identifier) = assignmentRhs.astChildren.drop(3).take(1).l
      fourthBlockChild.code shouldBe "tmp_1"
      fourthBlockChild.typeFullName shouldBe "mypkg.AClass"
      firstBlockChild.referencingIdentifiers.id.l.contains(fourthBlockChild.id) shouldBe true
    }
  }

}
