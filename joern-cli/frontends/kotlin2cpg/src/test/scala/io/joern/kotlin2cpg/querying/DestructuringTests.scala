package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
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
        |    val (myA, myB) = AClass("AMESSAGE", 41414141)
        |    println(myA)
        |    println(myB)
        |// prints
        |//```
        |//AMESSAGE
        |//41414141
        |//```
        |}
        |""".stripMargin)

    // TODO: add the test cases
  }
}
