// This test file has been translated from swift/test/Parse/borrow_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class BorrowExprTests extends AstSwiftSrc2CpgSuite {

  "BorrowExprTests" should {

    "testBorrowExpr1" in {
      val cpg = code("""
        |func useString(_ str: String) {}
        |var global: String = "123"
        |func testGlobal() {
        |  useString(_borrow global)
        |}
        |""".stripMargin)
      cpg.call.nameExact("useString").argument(1).isIdentifier.name.l shouldBe List("global")
    }

    "testBorrowExpr2" in {
      val cpg = code("""
        |func useString(_ str: String) {}
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  useString(_borrow t)
        |}
        |""".stripMargin)
      cpg.call.nameExact("useString").argument(1).isIdentifier.name.l shouldBe List("t")
    }
  }

}
