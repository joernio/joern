// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.semanticcpg.language.*

class GuardTests extends SwiftSrc2CpgSuite {

  "GuardTests" should {

    "testGuard1" in {
      val cpg = code("""
        |func noConditionNoElse() {
        |  guard {} else {}
        |}
        |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("noConditionNoElse").block.l
      val List(guardIf)     = methodBlock.astChildren.isControlStructure.l
      guardIf.order shouldBe 1
      guardIf.code shouldBe "guard {} else {}"
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("<lambda>0")
      guardIf.whenTrue.astChildren.code.l shouldBe empty
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe empty
    }

    "testGuard2" in {
      val cpg = code("""
        |var i = 2
        |while (i <= 10) {
        |  guard i % 2 == 0 else {
        |    i = i + 1
        |    continue
        |  }
        |  print(i)
        |  i = i + 1
        |} 
        |""".stripMargin)
      val List(whileBlock) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).whenTrue.l
      val List(guardIf)    = whileBlock.astChildren.isControlStructure.l
      guardIf.order shouldBe 1
      guardIf.code should startWith("guard i % 2 == 0")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("i % 2 == 0")
      guardIf.whenTrue.astChildren.code.l shouldBe List("print(i)", "i = i + 1")
      whileBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List("i = i + 1", "continue")
    }

    "testGuard3" in {
      val cpg = code("""
        |func checkOddEven() {
        |  var number = 24
        |  guard number % 2 == 0 else {
        |    print("Odd Number")
        |    return
        |  }
        |  print("Even Number")
        |}
        |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("checkOddEven").block.l
      val List(call)        = methodBlock.astChildren.isCall.l
      call.order shouldBe 1
      call.code shouldBe "var number = 24"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.order shouldBe 2
      guardIf.code should startWith("guard number % 2 == 0")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("number % 2 == 0")
      guardIf.whenTrue.code.l shouldBe List("print(\"Even Number\")")
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List(
        "print(\"Odd Number\")",
        "return"
      )
    }

    "testGuard4" in {
      val cpg = code("""
        |func checkJobEligibility() {
        |  var age = 33
        |  guard age >= 18, age <= 40 else {
        |    print("Not Eligible for Job")
        |    return
        |  }
        |  print("You are eligible for this job")
        |}
        |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("checkJobEligibility").block.l
      val List(call)        = methodBlock.astChildren.isCall.l
      call.order shouldBe 1
      call.code shouldBe "var age = 33"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.order shouldBe 2
      guardIf.code should startWith("guard age >= 18, age <= 40")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.astChildren.code.l shouldBe List("age >= 18", "age <= 40")
      guardIf.whenTrue.code.l shouldBe List("print(\"You are eligible for this job\")")
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren.code.l shouldBe List(
        "print(\"Not Eligible for Job\")",
        "return"
      )
    }

    "testGuard5" in {
      val cpg = code("""
        |func checkAge() {
        |  var age: Int? = 22
        |  guard let myAge = age else {
        |    print("Age is undefined")
        |    return
        |  }
        |  print("My age is \(myAge)")
        |}
        |""".stripMargin)
      val List(methodBlock) = cpg.method.nameExact("checkAge").block.l
      // After desugaring: age in method block, <tmp>0 in condition block, myAge in then block
      methodBlock.local.name.l should contain("age")
      val List(call) = methodBlock.astChildren.isCall.l
      call.order shouldBe 1
      call.code shouldBe "var age: Int? = 22"
      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.order shouldBe 2
      guardIf.code should startWith("guard let myAge = age")
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      // Condition is desugared to block with temp assignment and nil check
      val condBlock = guardIf.condition.isBlock.l
      condBlock should not be empty
      condBlock.head.local.name.l.exists(_.startsWith("<tmp>")) shouldBe true
      // Then block should have myAge local
      val thenBlock = guardIf.whenTrue.isBlock.l
      thenBlock should not be empty
      thenBlock.head.local.name.l should contain("myAge")
      guardIf.whenFalse.astChildren.code.l shouldBe List("print(\"Age is undefined\")", "return")
    }

    "testGuard6" in {
      val cpg = code("""
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
        |""".stripMargin)
      val List(methodBlock)  = cpg.method.nameExact("multipleLinear").block.l
      val List(callA, callB) = methodBlock.astChildren.isCall.l
      callA.order shouldBe 1
      callA.code shouldBe "var a = true"
      callB.order shouldBe 2
      callB.code shouldBe "var b = true"
      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.order shouldBe 3
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

    "testGuard7" in {
      val cpg = code("""
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
        |""".stripMargin)
      val List(methodBlock)  = cpg.method.nameExact("multipleNested").block.l
      val List(callA, callB) = methodBlock.astChildren.isCall.l
      callA.order shouldBe 1
      callA.code shouldBe "var a = true"
      callB.order shouldBe 2
      callB.code shouldBe "var b = true"
      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.order shouldBe 3
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

    "testGuardLet" in {
      val cpg = code("""
      |func test(optionalValue: Int?) {
      |  guard let value = optionalValue else {
      |    return
      |  }
      |  print(value)
      |}
      |""".stripMargin)

      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: desugared to { <tmp>0 = optionalValue; <tmp>0 != nil }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      tmpLocal.name should startWith("<tmp>")
      val tmpName = tmpLocal.name

      val List(condAssign) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      condAssign.code shouldBe s"$tmpName = optionalValue"
      condAssign.argument(1).code shouldBe tmpName
      condAssign.argument(2).code shouldBe "optionalValue"

      val List(condCheck) = condBlock.astChildren.isCall.nameExact(Operators.notEquals).l
      condCheck.code shouldBe s"$tmpName != nil"
      condCheck.argument(1).code shouldBe tmpName
      condCheck.argument(2).code shouldBe "nil"

      // Then block: { let value = <tmp>0 }
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(valueLocal) = thenBlock.astChildren.isLocal.l
      valueLocal.name shouldBe "value"

      val List(thenAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      thenAssign.code shouldBe s"value = $tmpName"
      thenAssign.argument(1).code shouldBe "value"
      thenAssign.argument(2).code shouldBe tmpName
    }

    "testGuardLetWithoutInitializer" in {
      val cpg = code("""
      |func test(optionalValue: Int?) {
      |  guard let optionalValue else {
      |    return
      |  }
      |  print(optionalValue)
      |}
      |""".stripMargin)

      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // For "guard let optionalValue" without explicit initializer:
      // Condition: optionalValue != nil (direct comparison, no block)
      val List(condCheck) = guardIf.condition.isCall.l
      condCheck.name shouldBe Operators.notEquals
      condCheck.code shouldBe "optionalValue != nil"
      condCheck.argument(1).code shouldBe "optionalValue"
      condCheck.argument(2).code shouldBe "nil"

      // Then branch: empty block (no new local or assignment for optionalValue)
      val thenNodes = guardIf.whenTrue.l
      thenNodes should not be empty

      // Verify no new local named "optionalValue" was created in the then branch
      val localsInThen = thenNodes.flatMap(_.ast.isLocal.nameExact("optionalValue").l)
      localsInThen shouldBe empty

      // Verify no assignment to optionalValue in the then branch
      val assignmentsInThen        = thenNodes.flatMap(_.ast.isCall.nameExact(Operators.assignment).l)
      val optionalValueAssignments = assignmentsInThen.filter(_.argument(1).code == "optionalValue")
      optionalValueAssignments shouldBe empty
    }

    "testGuardLetMultipleBindings" in {
      val cpg = code("""
      |func test() {
      |  guard let a = foo(), let b = bar() else {
      |    return
      |  }
      |  print(a, b)
      |}
      |""".stripMargin)

      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: desugared to { <tmp>0 = foo(); <tmp>1 = bar(); <tmp>0 != nil && <tmp>1 != nil }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmp0, tmp1) = condBlock.astChildren.isLocal.l
      tmp0.name shouldBe "<tmp>0"
      tmp1.name shouldBe "<tmp>1"

      val List(tmp0Assign, tmp1Assign) = condBlock.astChildren.assignment.l
      tmp0Assign.code shouldBe s"${tmp0.name} = foo()"
      tmp1Assign.code shouldBe s"${tmp1.name} = bar()"

      val List(andCheck)             = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(tmp0Check, tmp1Check) = andCheck.argument.isCall.nameExact(Operators.notEquals).l
      tmp0Check.code shouldBe s"${tmp0.name} != nil"
      tmp1Check.code shouldBe s"${tmp1.name} != nil"

      // Then block: { let a = <tmp>0; let b = <tmp>1 }
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      thenBlock.astChildren.isLocal.name.sorted shouldBe List("a", "b")

      val List(aAssignment, bAssignment) = thenBlock.astChildren.assignment.l
      aAssignment.code shouldBe s"a = ${tmp0.name}"
      bAssignment.code shouldBe s"b = ${tmp1.name}"
    }

    "testGuardLetMixedWithAndWithoutInitializer" in {
      val cpg = code("""
      |func test(existing: Int?) {
      |  guard let a = foo(), let existing else {
      |    return
      |  }
      |  print(a, existing)
      |}
      |""".stripMargin)

      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { <tmp>0 = foo(); <tmp>0 != nil && existing != nil }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      val tmpName        = tmpLocal.name
      tmpName should startWith("<tmp>")

      val List(tmpAssign) = condBlock.astChildren.isCall.nameExact(Operators.assignment).l
      tmpAssign.code shouldBe s"$tmpName = foo()"

      val List(andCheck)      = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val List(tmpCheck)      = andCheck.arguments(1).isCall.nameExact(Operators.notEquals).l
      val List(existingCheck) = andCheck.arguments(2).isCall.nameExact(Operators.notEquals).l
      tmpCheck.code shouldBe s"$tmpName != nil"
      existingCheck.code shouldBe "existing != nil"

      // Then block: { let a = <tmp>0 } (no assignment for 'existing')
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"

      val List(thenAssign) = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
      thenAssign.code shouldBe s"a = $tmpName"
    }

    "testGuardLetWithOtherConditions" in {
      val cpg = code("""
      |func test(flag: Bool) {
      |  guard let a = foo(), flag else {
      |    return
      |  }
      |  print(a)
      |}
      |""".stripMargin)

      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l

      // Condition: { <tmp>0 = foo(); <tmp>0 != nil && flag }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      val tmpName        = tmpLocal.name

      val List(andCheck) = condBlock.astChildren.isCall.nameExact(Operators.logicalAnd).l
      val arguments      = andCheck.argument.l
      arguments should have size 2

      // One should be the nil check, the other should be the flag identifier
      val nilChecks =
        arguments.collect { case c if c.isCall => c }.flatMap(_.ast.isCall.nameExact(Operators.notEquals).l)
      val flags = arguments.collect { case i if i.isIdentifier => i }.flatMap(_.ast.isIdentifier.nameExact("flag").l)

      nilChecks.code.l should contain(s"$tmpName != nil")
      flags should not be empty
    }

  }

}
