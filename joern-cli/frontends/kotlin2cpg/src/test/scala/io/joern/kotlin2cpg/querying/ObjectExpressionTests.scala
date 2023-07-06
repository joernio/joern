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
      val List(firstTd: TypeDecl, secondTd: TypeDecl) = cpg.typeDecl.isExternal(false).nameNot("<global>").l
      firstTd.fullName shouldBe "mypkg.foo$object$1"
      secondTd.fullName shouldBe "mypkg.foo$object$2"
    }

    "should contain two LOCAL nodes with the correct props set" in {
      val List(firstL: Local, secondL: Local) = cpg.local.l
      firstL.typeFullName shouldBe "mypkg.foo$object$1"
      secondL.typeFullName shouldBe "mypkg.foo$object$2"
    }

    "should contain two correctly-lowered representations of the assignments" in {
      val List(firstAssignment: Call, secondAssignment: Call) =
        cpg.method.nameExact("foo").call.methodFullNameExact("<operator>.assignment").l
      val List(firstAssignmentLHS: Identifier, firstAssignmentRHS: Call) = firstAssignment.argument.l: @unchecked
      firstAssignmentLHS.typeFullName shouldBe "mypkg.foo$object$1"
      firstAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
      val List(secondAssignmentLHS: Identifier, secondAssignmentRHS: Call) = secondAssignment.argument.l: @unchecked
      secondAssignmentLHS.typeFullName shouldBe "mypkg.foo$object$2"
      secondAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
    }

    "should contain two correctly-lowered calls to methods of the anonymous objects" in {
      val List(firstCall: Call, secondCall: Call) = cpg.call.methodFullName(".*printWithSuffix.*").l
      firstCall.methodFullName shouldBe "mypkg.foo$object$1.printWithSuffix:void(java.lang.String)"
      secondCall.methodFullName shouldBe "mypkg.foo$object$2.printWithSuffix:void(java.lang.String)"
      val List(firstCallLHS: Identifier, _: Identifier) = firstCall.argument.l: @unchecked
      firstCallLHS.typeFullName shouldBe "mypkg.foo$object$1"
      val List(secondCallLHS: Identifier, _: Identifier) = secondCall.argument.l: @unchecked
      secondCallLHS.typeFullName shouldBe "mypkg.foo$object$2"
    }
  }

  "CPG for code with object expressions defined inline as argument of a call" should {
    val cpg = code("""
        |package mypkg
        |interface AnInterface { fun doSomething(x: String) }
        |fun does(x: AnInterface, p: String) {
        |    x.doSomething(p)
        |}
        |fun foo(p: String) {
        |    does(object : AnInterface { override fun doSomething(x: String) { println(x) }}, p)
        |}
        |""".stripMargin)

    "contain a CALL node with the correct props set" in {
      val List(c: Call) = cpg.call.code("does.*").l
      c.methodFullName shouldBe "mypkg.does:void(mypkg.AnInterface,java.lang.String)"
    }

    "contain a TYPE_DECL node with the correct props set" in {
      val List(_: TypeDecl, secondTd: TypeDecl) = cpg.typeDecl.isExternal(false).nameNot("<global>").l
      secondTd.name shouldBe "anonymous_obj"
      secondTd.fullName shouldBe "mypkg.foo$object$1"
      secondTd.inheritsFromTypeFullName shouldBe Seq("mypkg.AnInterface")
      val List(firstMethod: Method, secondMethod: Method) = secondTd.boundMethod.l
      firstMethod.fullName shouldBe "mypkg.foo$object$1.doSomething:void(java.lang.String)"
      secondMethod.fullName shouldBe "mypkg.foo$object$1.<init>:void()"
    }

    "contain a LOCAL node with the correct props set" in {
      val List(l: Local) = cpg.local.l
      l.name shouldBe "tmp_obj_1"
      l.typeFullName shouldBe "mypkg.foo$object$1"
    }

    "contain a CALL node assigning a temp identifier to an alloc call" in {
      val List(firstAssignment: Call) = cpg.call.methodFullNameExact("<operator>.assignment").l
      val List(firstAssignmentLHS: Identifier, firstAssignmentRHS: Call) = firstAssignment.argument.l: @unchecked
      firstAssignmentLHS.typeFullName shouldBe "mypkg.foo$object$1"
      firstAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
    }

    "contain a CALL node for an <init> on the temp identifier" in {
      val List(c: Call) = cpg.call.nameExact("<init>").l
      c.methodFullName shouldBe "mypkg.foo$object$1.<init>:void()"
    }

    "contain an IDENTIFIER node for the argument representing the object literal" in {
      val List(firstArg: Identifier, _: Identifier) = cpg.call.code("does.*").argument.l: @unchecked
      firstArg.name shouldBe "tmp_obj_1"
      firstArg.typeFullName shouldBe "mypkg.foo$object$1"
    }
  }
}
