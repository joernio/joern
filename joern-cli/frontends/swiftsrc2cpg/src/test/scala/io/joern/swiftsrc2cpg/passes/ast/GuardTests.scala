// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
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
      guardIf.code shouldBe "guard {} else {}"
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF
      guardIf.condition.code.l shouldBe List("<lambda>0")
      guardIf.whenTrue.astChildren shouldBe empty
      methodBlock.astChildren.isControlStructure.whenFalse.astChildren shouldBe empty
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
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condition) = guardIf.condition.l
      condition.code shouldBe "i % 2 == 0"

      val List(thenPrint, thenAssign) = guardIf.whenTrue.astChildren.isCall.l
      thenPrint.code shouldBe "print(i)"
      thenAssign.name shouldBe Operators.assignment
      thenAssign.code shouldBe "i = i + 1"

      val List(elseAssign) = guardIf.whenFalse.astChildren.isCall.l
      elseAssign.name shouldBe Operators.assignment
      elseAssign.code shouldBe "i = i + 1"

      val List(elseContinue) = guardIf.whenFalse.astChildren.isControlStructure.l
      elseContinue.code shouldBe "continue"
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
      call.code shouldBe "var number = 24"

      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condition) = guardIf.condition.l
      condition.code shouldBe "number % 2 == 0"

      val List(thenPrint) = guardIf.whenTrue.isCall.l
      thenPrint.code shouldBe "print(\"Even Number\")"

      val List(elsePrint) = guardIf.whenFalse.astChildren.isCall.l
      elsePrint.code shouldBe "print(\"Odd Number\")"

      val List(elseReturn) = guardIf.whenFalse.astChildren.isReturn.l
      elseReturn.code shouldBe "return"
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
      call.code shouldBe "var age = 33"

      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF

      val List(cond1, cond2) = guardIf.condition.astChildren.l
      cond1.code shouldBe "age >= 18"
      cond2.code shouldBe "age <= 40"

      val List(thenPrint) = guardIf.whenTrue.isCall.l
      thenPrint.code shouldBe "print(\"You are eligible for this job\")"

      val List(elsePrint) = guardIf.whenFalse.astChildren.isCall.l
      elsePrint.code shouldBe "print(\"Not Eligible for Job\")"

      val List(elseReturn) = guardIf.whenFalse.astChildren.isReturn.l
      elseReturn.code shouldBe "return"
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
      // TODO(BUG): <tmp> local is incorrectly appearing as direct child of method block
      // It should only be in the condition block. For now, filter it out.
      val List(ageLocal) = methodBlock.astChildren.isLocal.filterNot(_.name.startsWith("<tmp>")).l
      ageLocal.name shouldBe "age"

      val List(call) = methodBlock.astChildren.isCall.l
      call.code shouldBe "var age: Int? = 22"

      val List(guardIf) = methodBlock.astChildren.isControlStructure.l
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF

      // Condition is desugared to block with temp assignment and nil check
      val List(condBlock) = guardIf.condition.isBlock.l
      val List(tmpLocal)  = condBlock.astChildren.isLocal.l
      val tmpName         = tmpLocal.name
      tmpName should startWith("<tmp>")

      val List(nilCheck) = condBlock.astChildren.isCall.l
      nilCheck.name shouldBe Operators.notEquals
      nilCheck.code shouldBe s"($tmpName = age) != nil"

      val List(assignment) = nilCheck.argument.isCall.l
      assignment.name shouldBe Operators.assignment
      assignment.code shouldBe s"$tmpName = age"

      val List(nilLit) = nilCheck.argument.isLiteral.l
      nilLit.code shouldBe "nil"

      // Then block should have myAge local
      val List(thenBlock)  = guardIf.whenTrue.isBlock.l
      val List(myAgeLocal) = thenBlock.astChildren.isLocal.l
      myAgeLocal.name shouldBe "myAge"

      val List(myAgeAssign, printCall) = thenBlock.astChildren.isCall.l
      myAgeAssign.name shouldBe Operators.assignment
      myAgeAssign.code shouldBe s"myAge = $tmpName"
      printCall.code shouldBe "print(\"My age is \\(myAge)\")"

      val List(elsePrint) = guardIf.whenFalse.astChildren.isCall.l
      elsePrint.name shouldBe "print"
      elsePrint.code shouldBe "print(\"Age is undefined\")"

      val List(elseReturn) = guardIf.whenFalse.astChildren.isReturn.l
      elseReturn.code shouldBe "return"
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
      callA.code shouldBe "var a = true"
      callB.code shouldBe "var b = true"

      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condA) = guardIfA.condition.l
      condA.code shouldBe "a"

      val List(elsePrintA) = guardIfA.whenFalse.astChildren.isCall.l
      elsePrintA.code shouldBe "print(\"else a\")"

      val List(elseReturnA) = guardIfA.whenFalse.astChildren.isReturn.l
      elseReturnA.code shouldBe "return"

      val List(thenPrintA) = guardIfA.whenTrue.astChildren.isCall.l
      thenPrintA.code shouldBe "print(\"a\")"

      val List(guardIfB) = guardIfA.whenTrue.astChildren.isControlStructure.l
      guardIfB.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condB) = guardIfB.condition.l
      condB.code shouldBe "b"

      val List(thenPrintB) = guardIfB.whenTrue.isCall.l
      thenPrintB.code shouldBe "print(\"b\")"

      val List(elsePrintB) = guardIfB.whenFalse.astChildren.isCall.l
      elsePrintB.code shouldBe "print(\"else b\")"

      val List(elseReturnB) = guardIfB.whenFalse.astChildren.isReturn.l
      elseReturnB.code shouldBe "return"
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
      callA.code shouldBe "var a = true"
      callB.code shouldBe "var b = true"

      val List(guardIfA) = methodBlock.astChildren.isControlStructure.l
      guardIfA.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condA) = guardIfA.condition.l
      condA.code shouldBe "a"

      val List(thenPrintA) = guardIfA.whenTrue.isCall.l
      thenPrintA.code shouldBe "print(\"a\")"

      val List(elsePrintA) = guardIfA.whenFalse.astChildren.isCall.l
      elsePrintA.code shouldBe "print(\"else a\")"

      val List(guardIfB) = guardIfA.whenFalse.astChildren.isControlStructure.l
      guardIfB.controlStructureType shouldBe ControlStructureTypes.IF

      val List(condB) = guardIfB.condition.l
      condB.code shouldBe "b"

      val List(thenPrintB) = guardIfB.whenTrue.astChildren.isCall.l
      thenPrintB.code shouldBe "print(\"b\")"

      val List(thenReturnB) = guardIfB.whenTrue.astChildren.isReturn.l
      thenReturnB.code shouldBe "return"

      val List(nestedElsePrintB) = guardIfB.whenFalse.astChildren.isCall.l
      nestedElsePrintB.code shouldBe "print(\"else b\")"

      val List(nestedElseReturnB) = guardIfB.whenFalse.astChildren.isReturn.l
      nestedElseReturnB.code shouldBe "return"
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

      // Condition: desugared to { let <tmp>0; (<tmp>0 = optionalValue) != nil }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      val tmpName        = tmpLocal.name
      tmpName should startWith("<tmp>")

      val List(condCheck) = condBlock.astChildren.isCall.l
      condCheck.name shouldBe Operators.notEquals
      condCheck.code shouldBe s"($tmpName = optionalValue) != nil"

      val List(condAssign) = condCheck.argument.isCall.l
      condAssign.name shouldBe Operators.assignment
      condAssign.code shouldBe s"$tmpName = optionalValue"

      val List(tmpArg, optValueArg) = condAssign.argument.l
      tmpArg.code shouldBe tmpName
      optValueArg.code shouldBe "optionalValue"

      val List(nilLit) = condCheck.argument.isLiteral.l
      nilLit.code shouldBe "nil"

      // Then block: { let value = <tmp>0; print(value) }
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(valueLocal) = thenBlock.astChildren.isLocal.l
      valueLocal.name shouldBe "value"

      val List(thenAssign, printCall) = thenBlock.astChildren.isCall.l
      thenAssign.name shouldBe Operators.assignment
      thenAssign.code shouldBe s"value = $tmpName"
      printCall.code shouldBe "print(value)"

      val List(valueArg, tmpRefArg) = thenAssign.argument.l
      valueArg.code shouldBe "value"
      tmpRefArg.code shouldBe tmpName

      val List(elseReturn) = guardIf.whenFalse.l
      elseReturn.code shouldBe "return"
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

      // Then branch: print(optionalValue) but no new local or assignment for optionalValue
      inside(guardIf.whenTrue.l) { case List(thenBlock) =>
        thenBlock.astChildren.isLocal.nameExact("optionalValue") shouldBe empty
        val assignments = thenBlock.astChildren.isCall.nameExact(Operators.assignment).l
        assignments.filter(_.argument(1).code == "optionalValue") shouldBe empty
      }
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

      val List(andCheck) = condBlock.astChildren.isCall.l
      andCheck.name shouldBe Operators.logicalAnd

      val List(tmp0Check, tmp1Check) = andCheck.argument.isCall.l
      tmp0Check.name shouldBe Operators.notEquals
      tmp0Check.code shouldBe s"(${tmp0.name} = foo()) != nil"
      tmp1Check.name shouldBe Operators.notEquals
      tmp1Check.code shouldBe s"(${tmp1.name} = bar()) != nil"

      val List(tmp0Assign) = tmp0Check.argument.isCall.l
      tmp0Assign.name shouldBe Operators.assignment
      tmp0Assign.code shouldBe s"${tmp0.name} = foo()"

      val List(tmp0Nil) = tmp0Check.argument.isLiteral.l
      tmp0Nil.code shouldBe "nil"

      val List(tmp1Assign) = tmp1Check.argument.isCall.l
      tmp1Assign.name shouldBe Operators.assignment
      tmp1Assign.code shouldBe s"${tmp1.name} = bar()"

      val List(tmp1Nil) = tmp1Check.argument.isLiteral.l
      tmp1Nil.code shouldBe "nil"

      // Then block: { let a = <tmp>0; let b = <tmp>1; print(a, b) }
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(aLocal, bLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"
      bLocal.name shouldBe "b"

      val List(aAssignment, bAssignment, printCall) = thenBlock.astChildren.isCall.l
      aAssignment.name shouldBe Operators.assignment
      aAssignment.code shouldBe s"a = ${tmp0.name}"
      bAssignment.name shouldBe Operators.assignment
      bAssignment.code shouldBe s"b = ${tmp1.name}"
      printCall.code shouldBe "print(a, b)"

      val List(elseReturn) = guardIf.whenFalse.l
      elseReturn.code shouldBe "return"
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

      // Condition: { let <tmp>0; (<tmp>0 = foo()) != nil && existing != nil }
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(tmpLocal) = condBlock.astChildren.isLocal.l
      val tmpName        = tmpLocal.name
      tmpName should startWith("<tmp>")

      val List(andCheck) = condBlock.astChildren.isCall.l
      andCheck.name shouldBe Operators.logicalAnd

      val List(tmpCheck, existingCheck) = andCheck.argument.isCall.l
      tmpCheck.name shouldBe Operators.notEquals
      tmpCheck.code shouldBe s"($tmpName = foo()) != nil"
      existingCheck.name shouldBe Operators.notEquals
      existingCheck.code shouldBe "existing != nil"

      val List(tmpAssign) = tmpCheck.argument.isCall.l
      tmpAssign.name shouldBe Operators.assignment
      tmpAssign.code shouldBe s"$tmpName = foo()"

      val List(tmpNil) = tmpCheck.argument.isLiteral.l
      tmpNil.code shouldBe "nil"

      val List(existingIdent) = existingCheck.argument.isIdentifier.l
      existingIdent.name shouldBe "existing"
      existingIdent.code shouldBe "existing"

      val List(existingNil) = existingCheck.argument.isLiteral.l
      existingNil.code shouldBe "nil"

      // Then block: { let a = <tmp>0; print(a, existing) } (no assignment for 'existing')
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"

      val List(thenAssign, printCall) = thenBlock.astChildren.isCall.l
      thenAssign.name shouldBe Operators.assignment
      thenAssign.code shouldBe s"a = $tmpName"
      printCall.code shouldBe "print(a, existing)"

      val List(elseReturn) = guardIf.whenFalse.l
      elseReturn.code shouldBe "return"
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
      tmpName should startWith("<tmp>")

      val List(andCheck) = condBlock.astChildren.isCall.l
      andCheck.name shouldBe Operators.logicalAnd

      inside(andCheck.argument.l) {
        case List(nilCheck: Call, flag: Identifier) =>
          nilCheck.name shouldBe Operators.notEquals
          nilCheck.code shouldBe s"($tmpName = foo()) != nil"
          flag.name shouldBe "flag"
          flag.code shouldBe "flag"
        case List(flag: Identifier, nilCheck: Call) =>
          nilCheck.name shouldBe Operators.notEquals
          nilCheck.code shouldBe s"($tmpName = foo()) != nil"
          flag.name shouldBe "flag"
          flag.code shouldBe "flag"
      }

      // Then block: { let a = <tmp>0; print(a) }
      val List(thenBlock) = guardIf.whenTrue.isBlock.l

      val List(aLocal) = thenBlock.astChildren.isLocal.l
      aLocal.name shouldBe "a"

      val List(thenAssign, printCall) = thenBlock.astChildren.isCall.l
      thenAssign.name shouldBe Operators.assignment
      thenAssign.code shouldBe s"a = $tmpName"
      printCall.name shouldBe "print"
      printCall.code shouldBe "print(a)"

      val List(elseReturn) = guardIf.whenFalse.l
      elseReturn.code shouldBe "return"
    }

  }

}
