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
        |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(localA)      = globalBlock.local.nameExact("a").l
      localA.typeFullName shouldBe "Swift.Int"
      val List(localB) = globalBlock.local.nameExact("b").l
      localB.typeFullName shouldBe "ANY"
      val assigns = cpg.call.nameExact(Operators.assignment).code.l
      assigns shouldBe List("let a: Int? = 1", "let b = a")
      val List(guardIf) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      guardIf.code should startWith("guard let b = a else")
    }
  }

}
