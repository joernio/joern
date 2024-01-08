// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class GuardTests extends AbstractPassTest {

  "GuardTests" should {

    "testGuard1" in AstFixture("""
        |func noConditionNoElse() {
        |  guard {} else {}
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock) = cpg.method.nameExact("noConditionNoElse").block.l
      val List(guardIf)     = methodBlock.astChildren.isControlStructure.l
      guardIf.argumentIndex shouldBe 1
      guardIf.code shouldBe "guard {} else {}"
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("{}")
      guardIf.whenTrue.code.l shouldBe empty
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe empty
    }

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
      guardIf.whenTrue.astChildren.code.l shouldBe List("print(i)", "i = i + 1")
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

    "testGuard6" in AstFixture("""
        |func multipleLinear() {
        |  var a = true
        |  var b = true
        |  guard a else {
        |    print("else a")
        |    return
        |  }
        |  print("a")
        |  guard b else {
        |    print("else b")
        |    return
        |  }
        |  print("b")
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock)  = cpg.method.nameExact("multipleLinear").block.l
      val List(callA, callB) = methodBlock.astChildren.isCall.l
      callA.argumentIndex shouldBe 1
      callA.code shouldBe "var a = true"
      callB.argumentIndex shouldBe 2
      callB.code shouldBe "var b = true"
      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.argumentIndex shouldBe 3
      guardIfA.code should startWith("guard a else")
      guardIfA.controlStructureType shouldBe ControlStructureTypes.IF
      guardIfA.condition.code.l shouldBe List("a")
      guardIfA.whenFalse.astChildren.code.l shouldBe List("print(\"else a\")", "return")
      guardIfA.whenTrue.astChildren.isCall.code.l shouldBe List("print(\"a\")")
      val List(guardIfB) = guardIfA.whenTrue.astChildren.isControlStructure.l
      guardIfB.code should startWith("guard b else")
      guardIfB.controlStructureType shouldBe ControlStructureTypes.IF
      guardIfB.condition.code.l shouldBe List("b")
      guardIfB.whenTrue.code.l shouldBe List("print(\"b\")")
      guardIfB.whenFalse.astChildren.code.l shouldBe List("print(\"else b\")", "return")
    }

    "testGuard7" in AstFixture("""
        |func multipleNested() {
        |  var a = true
        |  var b = true
        |  guard a else {
        |    print("else a")
        |    guard b else {
        |      print("else b")
        |      return
        |    }
        |    print("b")
        |    return
        |  }
        |  print("a")
        |}
        |""".stripMargin) { cpg =>
      val List(methodBlock)  = cpg.method.nameExact("multipleNested").block.l
      val List(callA, callB) = methodBlock.astChildren.isCall.l
      callA.argumentIndex shouldBe 1
      callA.code shouldBe "var a = true"
      callB.argumentIndex shouldBe 2
      callB.code shouldBe "var b = true"
      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.argumentIndex shouldBe 3
      guardIfA.code should startWith("guard a else")
      guardIfA.controlStructureType shouldBe ControlStructureTypes.IF
      guardIfA.condition.code.l shouldBe List("a")
      guardIfA.whenFalse.astChildren.isCall.code.l shouldBe List("print(\"else a\")")
      guardIfA.whenTrue.isCall.code.l shouldBe List("print(\"a\")")
      val List(guardIfB) = guardIfA.whenFalse.astChildren.isControlStructure.l
      guardIfB.code should startWith("guard b else")
      guardIfB.controlStructureType shouldBe ControlStructureTypes.IF
      guardIfB.condition.code.l shouldBe List("b")
      guardIfB.whenTrue.astChildren.code.l shouldBe List("print(\"b\")", "return")
      guardIfB.whenFalse.astChildren.code.l shouldBe List("print(\"else b\")", "return")
    }

  }

}
