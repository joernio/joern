// This test file has been translated from swift/test/Parse/guard-top-level.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
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
      val List(localA)      = globalBlock.local.nameExact("a").l
      localA.typeFullName shouldBe "Swift.Int"

      // After desugaring, b is in the guard's then block, not the global block
      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      guardIf.code should startWith("guard let b = a else")

      // Check that desugaring created the temp variable and nil check in condition
      val List(condBlock) = guardIf.condition.isBlock.l
      condBlock.local.name.l.exists(_.startsWith("<tmp>")) shouldBe true

      // Check that b local is in the then block along with code that follows the guard
      val List(thenBlock) = guardIf.whenTrue.isBlock.l
      val List(localB)    = thenBlock.local.nameExact("b").l
      localB.typeFullName shouldBe "ANY"

      // Verify the print(b) call is also in the then block (code following guard)
      val List(printCall) = thenBlock.astChildren.isCall.nameExact("print").l
      val List(bArg)      = printCall.argument.isIdentifier.l
      bArg.name shouldBe "b"
    }
  }

}
