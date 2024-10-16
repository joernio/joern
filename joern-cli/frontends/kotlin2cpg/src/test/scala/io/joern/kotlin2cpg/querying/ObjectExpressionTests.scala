package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.codepropertygraph.generated.nodes.Return
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
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

    "should contain TYPE_DECL nodes with the correct props set" in {
      val List(p1, p2, foo, o1, o2) = cpg.typeDecl.isExternal(false).nameNot("<global>").l
      p1.fullName shouldBe "mypkg.foo.object$0.printWithSuffix:void(java.lang.String)"
      p2.fullName shouldBe "mypkg.foo.object$1.printWithSuffix:void(java.lang.String)"
      foo.fullName shouldBe "mypkg.foo:void(java.lang.String)"
      o1.fullName shouldBe "mypkg.foo.object$0"
      o2.fullName shouldBe "mypkg.foo.object$1"
    }

    "should contain two LOCAL nodes with the correct props set" in {
      val List(firstL: Local, secondL: Local) = cpg.local.l
      firstL.typeFullName shouldBe "mypkg.foo.object$0"
      secondL.typeFullName shouldBe "mypkg.foo.object$1"
    }

    "should contain two correctly-lowered representations of the assignments" in {
      val List(firstAssignment: Call, secondAssignment: Call) =
        cpg.method.nameExact("foo").call.methodFullNameExact("<operator>.assignment").l
      val List(firstAssignmentLHS: Identifier, firstAssignmentRHS: Call) = firstAssignment.argument.l: @unchecked
      firstAssignmentLHS.typeFullName shouldBe "mypkg.foo.object$0"
      firstAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
      val List(secondAssignmentLHS: Identifier, secondAssignmentRHS: Call) = secondAssignment.argument.l: @unchecked
      secondAssignmentLHS.typeFullName shouldBe "mypkg.foo.object$1"
      secondAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
    }

    "should contain two correctly-lowered calls to methods of the anonymous objects" in {
      val List(firstCall: Call, secondCall: Call) = cpg.call.methodFullName(".*printWithSuffix.*").l
      firstCall.methodFullName shouldBe "mypkg.foo.object$0.printWithSuffix:void(java.lang.String)"
      secondCall.methodFullName shouldBe "mypkg.foo.object$1.printWithSuffix:void(java.lang.String)"
      val List(firstCallLHS: Identifier, _: Identifier) = firstCall.argument.l: @unchecked
      firstCallLHS.typeFullName shouldBe "mypkg.foo.object$0"
      val List(secondCallLHS: Identifier, _: Identifier) = secondCall.argument.l: @unchecked
      secondCallLHS.typeFullName shouldBe "mypkg.foo.object$1"
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

    "contain TYPE_DECL nodes with the correct props set" in {
      val List(f1, does, f2, foo, interface, obj) = cpg.typeDecl.isExternal(false).nameNot("<global>").l
      f1.fullName shouldBe "mypkg.AnInterface.doSomething:void(java.lang.String)"
      does.fullName shouldBe "mypkg.does:void(mypkg.AnInterface,java.lang.String)"
      f2.fullName shouldBe "mypkg.foo.object$0.doSomething:void(java.lang.String)"
      foo.fullName shouldBe "mypkg.foo:void(java.lang.String)"
      interface.fullName shouldBe "mypkg.AnInterface"
      interface.inheritsFromTypeFullName shouldBe List("java.lang.Object")
      obj.name shouldBe "anonymous_obj"
      obj.fullName shouldBe "mypkg.foo.object$0"
      obj.inheritsFromTypeFullName shouldBe Seq("mypkg.AnInterface")

      val List(firstMethod: Method, secondMethod: Method) = obj.boundMethod.l
      firstMethod.fullName shouldBe "mypkg.foo.object$0.doSomething:void(java.lang.String)"
      secondMethod.fullName shouldBe "mypkg.foo.object$0.<init>:void()"
    }

    "contain a LOCAL node with the correct props set" in {
      val List(l: Local) = cpg.local.l
      l.name shouldBe "tmp_obj_1"
      l.typeFullName shouldBe "mypkg.foo.object$0"
    }

    "contain a CALL node assigning a temp identifier to an alloc call" in {
      val List(firstAssignment: Call) = cpg.call.methodFullNameExact("<operator>.assignment").l
      val List(firstAssignmentLHS: Identifier, firstAssignmentRHS: Call) = firstAssignment.argument.l: @unchecked
      firstAssignmentLHS.typeFullName shouldBe "mypkg.foo.object$0"
      firstAssignmentRHS.methodFullName shouldBe "<operator>.alloc"
    }

    "contain a CALL node for an <init> on the temp identifier" in {
      val List(c: Call) = cpg.call.nameExact("<init>").l
      c.methodFullName shouldBe "mypkg.foo.object$0.<init>:void()"
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

    "contain TYPE_DECL nodes with the correct props set" in {
      val List(interfaceF1, objectF1, f1, interface, qClass, obj) = cpg.typeDecl.isExternal(false).nameNot("<global>").l
      interfaceF1.fullName shouldBe "mypkg.SomeInterface.doSomething:void()"
      objectF1.fullName shouldBe "mypkg.f1.object$0.doSomething:void()"
      f1.fullName shouldBe "mypkg.f1:void()"
      interface.fullName shouldBe "mypkg.SomeInterface"
      qClass.fullName shouldBe "mypkg.QClass"
      obj.name shouldBe "anonymous_obj"
      obj.fullName shouldBe "mypkg.f1.object$0"
      obj.inheritsFromTypeFullName shouldBe Seq("mypkg.SomeInterface")
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
      objExpr.fullName shouldBe "mypkg.withFailListener.object$0"
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
      objExpr.fullName shouldBe s"mypkg.f1.${Defines.ClosurePrefix}0.object$$1"
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
      cpg.typeDecl.fullNameExact("mypkg.AN_OBJ.object$0").l should not be List()
    }
  }
}
