// This test file has been translated from swift/test/Parse/borrow_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class BorrowExprTests extends AstSwiftSrc2CpgSuite {

  "BorrowExprTests" should {

    "testBorrowExpr1" ignore {
      val cpg = code("""
        |func useString(_ str: String) {}
        |var global: String = "123"
        |func testGlobal() {
        |  useString(_borrow global)
        |}
        |""".stripMargin)
      ???
    }

    "testBorrowExpr2" ignore {
      val cpg = code("""
        |func useString(_ str: String) {}
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  useString(_borrow t)
        |}
        |""".stripMargin)
      ???
    }
  }

}
