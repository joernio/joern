package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallsToContructorTests extends AnyFreeSpec with Matchers {

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

      val List(assignmentCall) = cpg.call.methodFullNameExact(Operators.assignment).l
      assignmentCall.signature shouldBe ""

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = assignmentCall.argument.l
      assignmentLhs.code shouldBe "f"
      assignmentLhs.name shouldBe "f"
      assignmentLhs.lineNumber shouldBe Some(6)
      assignmentLhs.columnNumber shouldBe Some(8)
      local.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true

      assignmentRhs.code shouldBe "alloc"
      assignmentRhs.signature shouldBe ""
      assignmentRhs.methodFullName shouldBe "<operator>.alloc"
      assignmentRhs.typeFullName shouldBe "java.io.File"
      assignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignmentRhs.argumentIndex shouldBe 2
      assignmentRhs.lineNumber shouldBe Some(6)
      assignmentRhs.columnNumber shouldBe Some(8)

      val List(c) = cpg.call.methodFullName(".*init.*").l
      c.name shouldBe "<init>"
      c.methodFullName shouldBe "java.io.File.<init>:void(kotlin.String)"
      c.signature shouldBe "void(kotlin.String)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(8)

      val List(firstArg: Identifier, secondArg: Literal) = c.argument.l
      firstArg.argumentIndex shouldBe 0
      firstArg.code shouldBe "f"
      firstArg.typeFullName shouldBe "java.io.File"
      local.referencingIdentifiers.id.l.contains(firstArg.id) shouldBe true

      secondArg.argumentIndex shouldBe 1
      secondArg.code shouldBe "\"/tmp/myfile.txt\""
      secondArg.typeFullName shouldBe "kotlin.String"
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

      val List(assignmentCall) = cpg.call.methodFullNameExact(Operators.assignment).l
      assignmentCall.signature shouldBe ""

      val List(assignmentLhs: Identifier, assignmentRhs: Call) = assignmentCall.argument.l
      assignmentLhs.code shouldBe "a"
      assignmentLhs.name shouldBe "a"
      assignmentLhs.lineNumber shouldBe Some(10)
      assignmentLhs.columnNumber shouldBe Some(8)
      local.referencingIdentifiers.id.l.contains(assignmentLhs.id) shouldBe true

      assignmentRhs.code shouldBe "alloc"
      assignmentRhs.signature shouldBe ""
      assignmentRhs.methodFullName shouldBe "<operator>.alloc"
      assignmentRhs.typeFullName shouldBe "mypkg.AClass"
      assignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignmentRhs.argumentIndex shouldBe 2
      assignmentRhs.lineNumber shouldBe Some(10)
      assignmentRhs.columnNumber shouldBe Some(8)

      val List(c) = cpg.call.methodFullName(".*init.*").l
      c.name shouldBe "<init>"
      c.methodFullName shouldBe "mypkg.AClass.<init>:void(kotlin.String)"
      c.signature shouldBe "void(kotlin.String)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(10)
      c.columnNumber shouldBe Some(8)

      val List(firstArg: Identifier, secondArg: Literal) = c.argument.l
      firstArg.argumentIndex shouldBe 0
      firstArg.code shouldBe "a"
      firstArg.typeFullName shouldBe "mypkg.AClass"
      local.referencingIdentifiers.id.l.contains(firstArg.id) shouldBe true

      secondArg.argumentIndex shouldBe 1
      secondArg.code shouldBe "\"AMESSAGE\""
      secondArg.typeFullName shouldBe "kotlin.String"
    }
  }
}
