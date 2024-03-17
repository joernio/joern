// This test file has been translated from swift/test/Parse/move_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class MoveExprTests extends AstSwiftSrc2CpgSuite {

  "MoveExprTests" should {

    "testMoveExpr1" ignore {
      val cpg = code("""
        |var global: Int = 5
        |let _ = _move global
        |""".stripMargin)
      ???
    }

    "testConsumeExpr1" ignore {
      val cpg = code("""
        |var global: Int = 5
        |let _ = consume global
        |""".stripMargin)
      ???
    }

  }

}
