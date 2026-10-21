// This test file has been translated from swift/test/Parse/move_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class MoveExprTests extends SwiftSrc2CpgSuite {

  "MoveExprTests" should {

    "testMoveExpr1" in {
      val cpg = code("""
        |var global: Int = 5
        |let _ = _move global
        |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(globalLocal) = globalBlock.local.nameExact("global").l
      globalLocal.typeFullName shouldBe "Swift.Int"
      val assigns = cpg.call.nameExact(Operators.assignment).code.l
      assigns shouldBe List("var global: Int = 5", "let <wildcard>0 = _move")
      // `_move` surfaces as an identifier in the SequenceExpr — there is no dedicated move/consume call.
      cpg.identifier.name.toSet should contain("_move")
    }

    "testConsumeExpr1" in {
      val cpg = code("""
        |var global: Int = 5
        |let _ = consume global
        |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").block.l
      val List(globalLocal) = globalBlock.local.nameExact("global").l
      globalLocal.typeFullName shouldBe "Swift.Int"
      val assigns = cpg.call.nameExact(Operators.assignment).code.l
      assigns shouldBe List("var global: Int = 5", "let <wildcard>0 = consume global")
    }

  }

}
