package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Local, Method, Return, TypeDecl}
import io.shiftleft.semanticcpg.language.*

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
  }

  "CPG for code with object expressions used as argument of a call in an assignment" should {
    val cpg = code("""
        |package mypkg
        |interface SomeInterface {
        |    fun doSomething()
        |}
        |class QClass(var o: SomeInterface)
        |fun f1() {
        |    var x: QClass
        |    x = QClass(object : SomeInterface { override fun doSomething() { println("something") } })
        |}
        |""".stripMargin)

    "contain a TYPE_DECL node with the correct props set" in {
      val List(_: TypeDecl, _: TypeDecl, thirdTd: TypeDecl) =
        cpg.typeDecl.isExternal(false).nameNot("<global>").l
      thirdTd.name shouldBe "anonymous_obj"
      thirdTd.fullName shouldBe "mypkg.f1$object$1"
      thirdTd.inheritsFromTypeFullName shouldBe Seq("mypkg.SomeInterface")
    }
  }

  "CPG for code with extension fn with single-expression body with object-expression inside it" should {
    val cpg = code("""
      |package mypkg
      |
      |interface SomeInterface {
      |    fun doSomething()
      |}
      |
      |class PClass {
      |    fun addListener(o: SomeInterface) {
      |        o.doSomething()
      |    }
      |}
      |
      |inline fun PClass.withFailListener(crossinline action: () -> Unit) =
      |    addListener(object : SomeInterface {
      |        override fun doSomething() {
      |            println("did something")
      |        }
      |    })
      | """.stripMargin)

    "should contain a correctly lowered representation" in {
      val List(last: Return) =
        cpg.method.nameExact("withFailListener").block.astChildren.isReturn.l

      val List(c: Call) = last.astChildren.isCall.l
      c.methodFullName shouldBe "mypkg.PClass.addListener:void(mypkg.SomeInterface)"
      val List(objExpr: TypeDecl, l: Local, alloc: Call, init: Call, i: Identifier) =
        c.astChildren.isBlock.astChildren.l: @unchecked
      objExpr.fullName shouldBe "mypkg.withFailListener$object$1"
      l.code shouldBe "tmp_obj_1"
      alloc.code shouldBe "tmp_obj_1 = <alloc>"
      init.code shouldBe "<init>"
      i.code shouldBe "tmp_obj_1"
    }
  }

  "CPG for lambda with a call return expression with object-expression as one of its arguments" should {
    val cpg = code("""
        |package mypkg
        |interface SomeInterface { fun doSomething() }
        |fun addListener(o: SomeInterface) { o.doSomething() }
        |fun f1() {
        |    val p = PClass()
        |    1.let {
        |        addListener(object : SomeInterface { override fun doSomething() { println("something") }})
        |    }
        |}
        | """.stripMargin)

    "should contain a correctly lowered representation" in {
      val List(_: Local, last: Return) =
        cpg.method.nameExact("<lambda>0").block.astChildren.l: @unchecked

      val List(c: Call) = last.astChildren.isCall.l
      c.methodFullName shouldBe "mypkg.addListener:void(mypkg.SomeInterface)"
      val List(objExpr: TypeDecl, l: Local, alloc: Call, init: Call, i: Identifier) =
        c.astChildren.isBlock.astChildren.l: @unchecked
      objExpr.fullName shouldBe "mypkg.f1$object$1"
      l.code shouldBe "tmp_obj_1"
      alloc.code shouldBe "tmp_obj_1 = <alloc>"
      init.code shouldBe "<init>"
      i.code shouldBe "tmp_obj_1"
    }
  }

  "CPG for code with object literal defined at the top-level of the package" should {
    val cpg = code("""
        |package mypkg
        |interface SomeInterface {
        |    fun doSomething()
        |}
        |val AN_OBJ = object : SomeInterface {
        |    override fun doSomething() {
        |        println("something")
        |    }
        |}
        | """.stripMargin)

    "contain a correctly lowered representation" in {
      cpg.typeDecl.fullNameExact("mypkg.AN_OBJ$object$1").l should not be List()
    }
  }
}
