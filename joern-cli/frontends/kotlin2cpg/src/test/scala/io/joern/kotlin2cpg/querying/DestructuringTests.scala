package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DestructuringTests extends AnyFreeSpec with Matchers {

  implicit val resolver = NoResolve

  "CPG for code with destructuring declaration and a variable as RHS" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aClass = AClass("AMESSAGE", 41414141)
        |    val (myA, myB) = aClass
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstAstConstruct)  = cpg.call.code("val.*aClass.*AClass.*").l
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"
      firstDestructLocal.order shouldBe (firstAstConstruct.order + 1)

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "java.lang.Integer"
      secondDestructLocal.order shouldBe (firstAstConstruct.order + 2)

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment
      firstDestructAssignment.order shouldBe (firstAstConstruct.order + 3)

      val List(firstDestructLHSIdentifier: Identifier) = firstDestructAssignment.argument(1).l
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCall: Call) = firstDestructAssignment.argument(2).l
      firstDestructRHSCall.code shouldBe "aClass.component1()"
      firstDestructRHSCall.methodFullName shouldBe "main.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code shouldBe "aClass"
      firstDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment
      secondDestructAssignment.order shouldBe (firstAstConstruct.order + 4)

      val List(secondDestructLHSIdentifier: Identifier) = secondDestructAssignment.argument(1).l
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "java.lang.Integer"

      val List(secondDestructRHSCall: Call) = secondDestructAssignment.argument(2).l
      secondDestructRHSCall.code shouldBe "aClass.component2()"
      secondDestructRHSCall.methodFullName shouldBe "main.AClass.component2:java.lang.Integer()"
      secondDestructRHSCall.signature shouldBe "java.lang.Integer()"
      secondDestructRHSCall.typeFullName shouldBe "java.lang.Integer"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code shouldBe "aClass"
      secondDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }

  "CPG for code with destructuring expression with a ctor-invocation RHS" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |data class AClass(val a: String, val b: Int)
        |fun main() {
        |    val aMessage = "AMESSAGE"
        |    val (myA, myB) = AClass(aMessage, 41414141)
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly-lowered representation" in {
      val List(firstAstConstruct)  = cpg.call.code("val aMessage.*=.*").l
      val List(firstDestructLocal) = cpg.local.nameExact("myA").l
      firstDestructLocal.code shouldBe "myA"
      firstDestructLocal.typeFullName shouldBe "java.lang.String"
      firstDestructLocal.order shouldBe (firstAstConstruct.order + 1)

      val List(secondDestructLocal) = cpg.local.nameExact("myB").l
      secondDestructLocal.code shouldBe "myB"
      secondDestructLocal.typeFullName shouldBe "java.lang.Integer"
      secondDestructLocal.order shouldBe (firstAstConstruct.order + 2)

      val List(tmpLocal) = cpg.local.name("tmp_.*").l
      tmpLocal.code shouldBe "tmp_1"
      tmpLocal.typeFullName shouldBe "main.AClass"
      tmpLocal.order shouldBe (firstAstConstruct.order + 4)

      val List(tmpAssignmentToAlloc) = cpg.call.code("tmp.*=.*alloc.*").l
      tmpAssignmentToAlloc.methodFullName shouldBe Operators.assignment
      tmpAssignmentToAlloc.typeFullName shouldBe ""
      tmpAssignmentToAlloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      tmpAssignmentToAlloc.order shouldBe (firstAstConstruct.order + 5)

      val List(allocAssignmentLhs: Identifier, allocAssignmentRhs: Call) = tmpAssignmentToAlloc.argument.l
      allocAssignmentLhs.code shouldBe "tmp_1"
      allocAssignmentLhs.typeFullName shouldBe "main.AClass"
      tmpLocal.referencingIdentifiers.id.l.contains(allocAssignmentLhs.id) shouldBe true

      allocAssignmentRhs.code shouldBe "alloc"
      allocAssignmentRhs.signature shouldBe ""
      allocAssignmentRhs.methodFullName shouldBe "<operator>.alloc"
      allocAssignmentRhs.typeFullName shouldBe "main.AClass"
      allocAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      allocAssignmentRhs.argumentIndex shouldBe 2

      val List(tmpInitCall) = cpg.call.code("tmp.*init.*").l
      tmpInitCall.order shouldBe (firstAstConstruct.order + 6)

      val List(initCallFirstArg: Identifier, initCallSecondArg: Identifier, initCallThirdArg: Literal) =
        tmpInitCall.argument.l
      initCallFirstArg.code shouldBe "tmp_1"
      initCallFirstArg.typeFullName shouldBe "main.AClass"
      initCallFirstArg.argumentIndex shouldBe 0
      initCallFirstArg.order shouldBe 1
      tmpLocal.referencingIdentifiers.id.l.contains(initCallFirstArg.id) shouldBe true

      initCallSecondArg.code shouldBe "aMessage"
      initCallSecondArg.typeFullName shouldBe "java.lang.String"
      initCallSecondArg.argumentIndex shouldBe 1
      initCallSecondArg.order shouldBe 2

      initCallThirdArg.code shouldBe "41414141"
      initCallThirdArg.typeFullName shouldBe "java.lang.Integer"
      initCallThirdArg.argumentIndex shouldBe 2
      initCallThirdArg.order shouldBe 3

      val List(firstDestructAssignment) = cpg.call.code("myA.*=.*component.*").l
      firstDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      firstDestructAssignment.methodFullName shouldBe Operators.assignment
      firstDestructAssignment.order shouldBe (firstAstConstruct.order + 7)

      val List(firstDestructLHSIdentifier: Identifier) = firstDestructAssignment.argument(1).l
      firstDestructLHSIdentifier.name shouldBe "myA"
      firstDestructLHSIdentifier.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCall: Call) = firstDestructAssignment.argument(2).l
      firstDestructRHSCall.code should startWith("tmp_")
      firstDestructRHSCall.code should endWith("component1()")
      firstDestructRHSCall.methodFullName shouldBe "main.AClass.component1:java.lang.String()"
      firstDestructRHSCall.signature shouldBe "java.lang.String()"
      firstDestructRHSCall.typeFullName shouldBe "java.lang.String"

      val List(firstDestructRHSCallArgument: Identifier) =
        firstDestructAssignment.argument(2).asInstanceOf[Call].argument.l
      firstDestructRHSCallArgument.argumentIndex shouldBe 0
      firstDestructRHSCallArgument.code should startWith("tmp_")
      firstDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      firstDestructRHSCallArgument.refsTo.size shouldBe 1

      val List(secondDestructAssignment) = cpg.call.code("myB.*=.*component.*").l
      secondDestructAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondDestructAssignment.methodFullName shouldBe Operators.assignment
      secondDestructAssignment.order shouldBe (firstAstConstruct.order + 8)

      val List(secondDestructLHSIdentifier: Identifier) = secondDestructAssignment.argument(1).l
      secondDestructLHSIdentifier.name shouldBe "myB"
      secondDestructLHSIdentifier.typeFullName shouldBe "java.lang.Integer"

      val List(secondDestructRHSCall: Call) = secondDestructAssignment.argument(2).l
      secondDestructRHSCall.code should startWith("tmp_")
      secondDestructRHSCall.code should endWith("component2()")
      secondDestructRHSCall.methodFullName shouldBe "main.AClass.component2:java.lang.Integer()"
      secondDestructRHSCall.signature shouldBe "java.lang.Integer()"
      secondDestructRHSCall.typeFullName shouldBe "java.lang.Integer"

      val List(secondDestructRHSCallArgument: Identifier) =
        secondDestructAssignment.argument(2).asInstanceOf[Call].argument.l
      secondDestructRHSCallArgument.argumentIndex shouldBe 0
      secondDestructRHSCallArgument.code should startWith("tmp_")
      secondDestructRHSCallArgument.typeFullName shouldBe "main.AClass"
      secondDestructRHSCallArgument.refsTo.size shouldBe 1
    }
  }
}
