package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ConstructorTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for a class declaration with an implicit constructor" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void()"
      m.name shouldBe "<init>"
      m.parameter.size shouldBe 1
      m.block.size shouldBe 1
    }
  }

  "CPG for code with class with param in its primary constructor" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(x: String)
        |""".stripMargin)

    "should contain a METHOD node for the constructor with a block with no children" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.AClass").method.l
      m.fullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      m.name shouldBe "<init>"
      m.parameter.size shouldBe 2
      m.block.size shouldBe 1
      m.block.expressionDown.size shouldBe 0
    }
  }

  "CPG for code with class which defines member in its primary constructor" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(val x: String)
        |
        |fun doSomething(p1: String): String {
        |    val aClass = AClass(p1)
        |    return aClass.x
        |}
        |
        |fun main() {
        |    val out = doSomething("AMESSAGE")
        |    println(out)
        |}
        |
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.AClass").method.l
      m.fullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      m.name shouldBe "<init>"
      m.parameter.size shouldBe 2
      m.block.size shouldBe 1

      val List(firstParam: MethodParameterIn, secondParam: MethodParameterIn) = m.parameter.l
      firstParam.name shouldBe "this"
      firstParam.index shouldBe 0
      secondParam.name shouldBe "x"
      secondParam.index shouldBe 1

      val List(memberAssignmentCall) = m.block.expressionDown.isCall.l
      memberAssignmentCall.code shouldBe "this.x = x"
      memberAssignmentCall.methodFullName shouldBe Operators.assignment
      memberAssignmentCall.name shouldBe Operators.assignment
      memberAssignmentCall.order shouldBe 1

      val List(assignmentLhs: Call, assignmentRhs: Identifier) = memberAssignmentCall.argument.l
      assignmentRhs.code shouldBe "x"
      assignmentRhs.name shouldBe "x"
      assignmentRhs.typeFullName shouldBe "java.lang.String"
      assignmentRhs.argumentIndex shouldBe 2
      assignmentRhs.order shouldBe 2

      val List(refParam: MethodParameterIn) = assignmentRhs.refsTo.l
      refParam.name shouldBe "x"

      assignmentLhs.methodFullName shouldBe Operators.fieldAccess
      assignmentLhs.name shouldBe Operators.fieldAccess
      assignmentLhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignmentLhs.typeFullName shouldBe "java.lang.String"
      assignmentLhs.order shouldBe 1
      assignmentLhs.argumentIndex shouldBe 1

      val List(fieldAccessLhs: Identifier, fieldAccessRhs: FieldIdentifier) = assignmentLhs.argument.l
      fieldAccessLhs.order shouldBe 1
      fieldAccessLhs.argumentIndex shouldBe 1
      fieldAccessLhs.typeFullName shouldBe "mypkg.AClass"
      fieldAccessLhs.name shouldBe "this"
      fieldAccessLhs.code shouldBe "this"

      fieldAccessRhs.order shouldBe 2
      fieldAccessRhs.argumentIndex shouldBe 2
      fieldAccessRhs.canonicalName shouldBe "x"
    }
  }

  "CPG for a class declaration with an implicit constructor with parameters" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo(bar: String) {
        |}
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String)"
      m.name shouldBe "<init>"
      m.parameter.size shouldBe 2
      m.block.size shouldBe 1
    }
  }

  "CPG for a class declaration with an explicit constructor with parameters" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo constructor(bar: String) {
        |}
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String)"
      m.name shouldBe "<init>"
      m.parameter.size shouldBe 2
      m.block.size shouldBe 1
    }
  }

  "CPG for a class declaration with secondary constructor" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |class Foo(foo: String) {
        |    var bar: Int = 0
        |    constructor(foo:String, bar: Int): this(foo) {
        |        this.bar = bar
        |    }
        |}
        |""".stripMargin)

    "should contain METHOD nodes for the primary and secondary constructors with the correct fullnames set" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.fullName.l shouldBe List(
        "mypkg.Foo.<init>:void(java.lang.String)",
        "mypkg.Foo.<init>:void(java.lang.String,java.lang.Integer)"
      )
    }

    "should contain a METHOD node for the primary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).parameter.size shouldBe 2
    }

    "should contain a METHOD node for the secondary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.drop(1).take(1).parameter.size shouldBe 3
    }

    "should contain a METHOD node for the primary constructor with properties set correctly" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).l
      m.lineNumber shouldBe Some(3)
      m.columnNumber shouldBe Some(9)
      m.methodReturn.code shouldBe "RET"
      m.methodReturn.lineNumber shouldBe Some(3)
      m.methodReturn.columnNumber shouldBe Some(9)
    }

    "should contain a METHOD node for the secondary constructor with properties set correctly" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.drop(1).take(1).l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String,java.lang.Integer)"
      m.lineNumber shouldBe Some(5)
      m.columnNumber shouldBe Some(4)
      m.methodReturn.code shouldBe "RET"
      m.methodReturn.lineNumber shouldBe Some(5)
      m.methodReturn.columnNumber shouldBe Some(4)
    }
  }
}
