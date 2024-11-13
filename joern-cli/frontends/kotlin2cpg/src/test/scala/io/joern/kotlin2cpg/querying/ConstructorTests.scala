package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*

class ConstructorTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for a class declaration with an implicit constructor" should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void()"
      m.name shouldBe Defines.ConstructorMethodName
      m.parameter.size shouldBe 1
      Option(m.block).isDefined shouldBe true
    }
  }

  "CPG for code with class with param in its primary constructor" should {
    val cpg = code("""
        |package mypkg
        |
        |class AClass(x: String)
        |""".stripMargin)

    "should contain a METHOD node for the constructor with a block with no children" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.AClass").method.l
      m.fullName shouldBe "mypkg.AClass.<init>:void(java.lang.String)"
      m.name shouldBe Defines.ConstructorMethodName
      m.parameter.size shouldBe 2
      Option(m.block).isDefined shouldBe true
      m.block.expressionDown.size shouldBe 0
    }
  }

  "CPG for code with class which defines member in its primary constructor" should {
    val cpg = code("""
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
      m.name shouldBe Defines.ConstructorMethodName
      m.parameter.size shouldBe 2
      Option(m.block).isDefined shouldBe true

      val List(firstParam: MethodParameterIn, secondParam: MethodParameterIn) = m.parameter.l
      firstParam.name shouldBe "this"
      firstParam.order shouldBe 0
      secondParam.name shouldBe "x"
      secondParam.order shouldBe 1

      val List(memberAssignmentCall) = m.block.expressionDown.isCall.l
      memberAssignmentCall.code shouldBe "this.x = x"
      memberAssignmentCall.methodFullName shouldBe Operators.assignment
      memberAssignmentCall.name shouldBe Operators.assignment
      memberAssignmentCall.order shouldBe 1

      val List(assignmentLhs: Call, assignmentRhs: Identifier) = memberAssignmentCall.argument.l: @unchecked
      assignmentRhs.code shouldBe "x"
      assignmentRhs.name shouldBe "x"
      assignmentRhs.typeFullName shouldBe "java.lang.String"
      assignmentRhs.argumentIndex shouldBe 2
      assignmentRhs.order shouldBe 2

      val List(refParam: MethodParameterIn) = assignmentRhs.refsTo.l: @unchecked
      refParam.name shouldBe "x"

      assignmentLhs.methodFullName shouldBe Operators.fieldAccess
      assignmentLhs.name shouldBe Operators.fieldAccess
      assignmentLhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignmentLhs.order shouldBe 1
      assignmentLhs.argumentIndex shouldBe 1

      val List(fieldAccessLhs: Identifier, fieldAccessRhs: FieldIdentifier) = assignmentLhs.argument.l: @unchecked
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

  "CPG for a class declaration with an implicit constructor with parameters" should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo(bar: String) {
        |}
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String)"
      m.name shouldBe Defines.ConstructorMethodName
      m.parameter.size shouldBe 2
      Option(m.block).isDefined shouldBe true
    }
  }

  "CPG for a class declaration with an explicit constructor with parameters" should {
    val cpg = code("""
        |package mypkg
        |
        |class Foo constructor(bar: String) {
        |}
        |""".stripMargin)

    "should contain a METHOD node for the constructor with the correct props set" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String)"
      m.name shouldBe Defines.ConstructorMethodName
      m.parameter.size shouldBe 2
      Option(m.block).isDefined shouldBe true
    }
  }

  "CPG for a class declaration with secondary constructor" should {
    val cpg = code("""
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
        "mypkg.Foo.<init>:void(java.lang.String,int)"
      )
    }

    "should contain a METHOD node for the primary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).parameter.size shouldBe 2
    }

    "should contain a METHOD node for the secondary constructor with the correct number of parameters" in {
      cpg.typeDecl.fullNameExact("mypkg.Foo").method.slice(1, 2).parameter.size shouldBe 3
    }

    "should contain a METHOD node for the primary constructor with properties set correctly" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.take(1).l
      m.lineNumber shouldBe Some(4)
      m.columnNumber shouldBe Some(9)
      m.methodReturn.typeFullName shouldBe "void"
      m.methodReturn.lineNumber shouldBe Some(4)
      m.methodReturn.columnNumber shouldBe Some(9)
    }

    "should contain a METHOD node for the secondary constructor with properties set correctly" in {
      val List(m) = cpg.typeDecl.fullNameExact("mypkg.Foo").method.slice(1, 2).l
      m.fullName shouldBe "mypkg.Foo.<init>:void(java.lang.String,int)"
      m.name shouldBe Defines.ConstructorMethodName
      m.lineNumber shouldBe Some(6)
      m.columnNumber shouldBe Some(4)
      m.methodReturn.typeFullName shouldBe "void"
      m.methodReturn.lineNumber shouldBe Some(6)
      m.methodReturn.columnNumber shouldBe Some(4)

      m.block.astChildren.map(_.code).l shouldBe List(Defines.ConstructorMethodName, "this.bar = bar")

      val List(mThisParam: MethodParameterIn, firstParam: MethodParameterIn, secondParam: MethodParameterIn) =
        m.parameter.l
      mThisParam.name shouldBe "this"
      firstParam.name shouldBe "foo"
      secondParam.name shouldBe "bar"

      val b                                     = m.block
      val List(_: Call, secondBlockChild: Call) = b.astChildren.l: @unchecked
      secondBlockChild.methodFullName shouldBe Operators.assignment
      secondBlockChild.code shouldBe "this.bar = bar"
      secondBlockChild.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(assignmentLhs: Call, assignmentRhs: Identifier) = secondBlockChild.argument.l: @unchecked
      assignmentLhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignmentLhs.methodFullName shouldBe Operators.fieldAccess
      assignmentLhs.name shouldBe Operators.fieldAccess
      assignmentRhs.code shouldBe "bar"
      secondParam.referencingIdentifiers.id.l.contains(assignmentRhs.id) shouldBe true
      val List(thisIdentifier: Identifier, relevantFieldIdentifier: FieldIdentifier) =
        assignmentLhs.argument.l: @unchecked
      thisIdentifier.code shouldBe "this"
      thisIdentifier.argumentIndex shouldBe 1
      mThisParam.referencingIdentifiers.id.l.contains(thisIdentifier.id) shouldBe true
      relevantFieldIdentifier.code shouldBe "bar"
      relevantFieldIdentifier.argumentIndex shouldBe 2
    }
  }
}
