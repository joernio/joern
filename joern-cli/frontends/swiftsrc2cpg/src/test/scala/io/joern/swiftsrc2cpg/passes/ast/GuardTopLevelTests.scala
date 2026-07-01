// This test file has been translated from swift/test/Parse/guard-top-level.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class GuardTopLevelTests extends SwiftSrc2CpgSuite {

  "GuardTopLevelTests" should {
    "testGuardTopLevel1" in {
      val cpg = code("""
        |let a: Int? = 1
        |guard let b = a else {}
        |print(b)
        |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l

      // After desugaring: <tmp>0 and `a` in global block
      val List(tmpLocal, localA) = globalBlock.local.l
      val tmpName                = tmpLocal.name
      tmpName shouldBe "<tmp>0"
      localA.name shouldBe "a"
      localA.typeFullName shouldBe "Swift.Int"

      // After desugaring, b is in the guard's then block, not the global block
      val List(guardIf: ControlStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      guardIf.code shouldBe "guard let b = a else {}"
      guardIf.controlStructureType shouldBe ControlStructureTypes.IF

      // Check that desugaring created the temp variable and nil check in condition
      val List(condBlock) = guardIf.condition.isBlock.l

      val List(nilCheck) = condBlock.astChildren.isCall.l
      nilCheck.name shouldBe Operators.notEquals
      nilCheck.code shouldBe s"($tmpName = a) != nil"

      val List(assignment) = nilCheck.argument.isCall.l
      assignment.name shouldBe Operators.assignment
      assignment.code shouldBe s"$tmpName = a"

      val List(tmpArg, aArg) = assignment.argument.l
      tmpArg.code shouldBe tmpName
      aArg.code shouldBe "a"

      val List(nilLit) = nilCheck.argument.isLiteral.l
      nilLit.code shouldBe "nil"

      // Check that b local is in the then block along with code that follows the guard
      val List(thenBlock) = guardIf.whenTrue.isBlock.l
      val List(localB)    = thenBlock.local.l
      localB.name shouldBe "b"

      // Verify the print(b) call is also in the then block (code following guard)
      val List(bAssign, printCall) = thenBlock.astChildren.isCall.l
      bAssign.name shouldBe Operators.assignment
      bAssign.code shouldBe s"b = $tmpName"
      printCall.code shouldBe "print(b)"

      val List(bArg) = printCall.argument.l
      bArg.code shouldBe "b"

      inside(guardIf.whenFalse.l) { case List(elseNode: ControlStructure) =>
        elseNode.controlStructureType shouldBe ControlStructureTypes.ELSE
        elseNode.astChildren.isBlock.astChildren shouldBe empty
      }
    }
  }

}
