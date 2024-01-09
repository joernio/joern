// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class GuardTests extends AbstractPassTest {

  "GuardTests" should {

    "testGuard1" ignore AstFixture("""
        |func noConditionNoElse() {
        |  guard {} else {}
        |}
        |""".stripMargin) { cpg => }

    "testGuard2" in AstFixture("""
        |var i = 2
        |while (i <= 10) {
        |  guard i % 2 == 0 else {
        |    i = i + 1
        |    continue
        |  }
        |  print(i)
        |  i = i + 1
        |} 
        |""".stripMargin) { cpg =>
      val List(whileBlock) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).whenTrue.l
      val List(guardIf)    = whileBlock.astChildren.isControlStructure.l
      guardIf.argumentIndex shouldBe 1
      guardIf.code should startWith("guard i % 2 == 0")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("i % 2 == 0")
      guardIf.whenTrue.code.l shouldBe List("print(i)", "i = i + 1")
      whileBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List("i = i + 1", "continue")
    }

    "testGuard3" in AstFixture("""
        |func checkOddEven() {
        |  var number = 24
        |  guard number % 2 == 0 else {
        |    print("Odd Number")
        |    return
        |  }
        |  print("Even Number")
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock) = cpg.method.nameExact("checkOddEven").block.l
      val List(call)        = methodBlock.astChildren.isCall.l
      call.argumentIndex shouldBe 1
      call.code shouldBe "var number = 24"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.argumentIndex shouldBe 2
      guardIf.code should startWith("guard number % 2 == 0")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("number % 2 == 0")
      guardIf.whenTrue.code.l shouldBe List("print(\"Even Number\")")
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List(
        "print(\"Odd Number\")",
        "return"
      )
    }

    "testGuard4" in AstFixture("""
        |func checkJobEligibility() {
        |  var age = 33
        |  guard age >= 18, age <= 40 else {
        |    print("Not Eligible for Job")
        |    return
        |  }
        |  print("You are eligible for this job")
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock) = cpg.method.nameExact("checkJobEligibility").block.l
      val List(call)        = methodBlock.astChildren.isCall.l
      call.argumentIndex shouldBe 1
      call.code shouldBe "var age = 33"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.argumentIndex shouldBe 2
      guardIf.code should startWith("guard age >= 18, age <= 40")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.astChildren.code.l shouldBe List("age >= 18", "age <= 40")
      guardIf.whenTrue.code.l shouldBe List("print(\"You are eligible for this job\")")
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List(
        "print(\"Not Eligible for Job\")",
        "return"
      )
    }

    "testGuard5" in AstFixture("""
        |func checkAge() {
        |  var age: Int? = 22
        |  guard let myAge = age else {
        |    print("Age is undefined")
        |    return
        |  }
        |  print("My age is \(myAge)")
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock) = cpg.method.nameExact("checkAge").block.l
      methodBlock.local.name.l shouldBe List("myAge", "age", "this", "print")
      val List(call) = methodBlock.astChildren.isCall.l
      call.argumentIndex shouldBe 1
      call.code shouldBe "var age: Int? = 22"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.argumentIndex shouldBe 2
      guardIf.code should startWith("guard let myAge = age")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("let myAge = age")
      guardIf.whenTrue.code.l shouldBe List("print(\"My age is \\(myAge)\")")
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List(
        "print(\"Age is undefined\")",
        "return"
      )
    }

  }

}
