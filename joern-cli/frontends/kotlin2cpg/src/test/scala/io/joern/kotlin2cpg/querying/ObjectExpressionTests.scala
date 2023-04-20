package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Local, Method, TypeDecl}
import io.shiftleft.semanticcpg.language._

class ObjectExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with two simple object expressions" should {
    val cpg = code("""
        |package mypkg
        |
        |fun foo(p: String) {
        |    val o = object {
        |        val m = "meow"
        |        fun printWithSuffix(suffix: String) {
        |            println(suffix + m)
        |        }
        |    }
        |    o.printWithSuffix(p)
        |
        |    val r = object {
        |         val m = "moo"
        |         fun printWithSuffix(suffix: String) {
        |             println(suffix + m)
        |         }
        |     }
        |     r.printWithSuffix(p)
        |}
        |""".stripMargin)

    "should contain two TYPE_DECL nodes with the correct props set" in {
      val List(firstTd: TypeDecl, secondTd: TypeDecl) = cpg.typeDecl.isExternal(false).l
      firstTd.fullName shouldBe "mypkg.foo$object$1"
      secondTd.fullName shouldBe "mypkg.foo$object$2"
    }

    "should contain two LOCAL nodes with the correct props set" in {
      val List(firstL: Local, secondL: Local) = cpg.local.l
      firstL.typeFullName shouldBe "mypkg.foo$object$1"
      secondL.typeFullName shouldBe "mypkg.foo$object$2"
    }

    "should contain two correctly-lowered representations of the assignments" in {
      val List(firstAssignment: Call, secondAssignment: Call) = cpg.call.methodFullNameExact("<operator>.assignment").l
      val List(firstAssignmentLHS: Identifier, firstAssignmentRHS: Call) = firstAssignment.argument.l
      firstAssignmentLHS.typeFullName shouldBe "mypkg.foo$object$1"
      firstAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
      val List(secondAssignmentLHS: Identifier, secondAssignmentRHS: Call) = secondAssignment.argument.l
      secondAssignmentLHS.typeFullName shouldBe "mypkg.foo$object$2"
      secondAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
    }

    "should contain two correctly-lowered calls to methods of the anonymous objects" in {
      val List(firstCall: Call, secondCall: Call) = cpg.call.methodFullName(".*printWithSuffix.*").l
      firstCall.methodFullName shouldBe "mypkg.foo$object$1.printWithSuffix:void(java.lang.String)"
      secondCall.methodFullName shouldBe "mypkg.foo$object$2.printWithSuffix:void(java.lang.String)"
      val List(firstCallLHS: Identifier, _: Identifier) = firstCall.argument.l
      firstCallLHS.typeFullName shouldBe "mypkg.foo$object$1"
      val List(secondCallLHS: Identifier, _: Identifier) = secondCall.argument.l
      secondCallLHS.typeFullName shouldBe "mypkg.foo$object$2"
    }
  }
}
