// This test file has been translated from swift/test/Parse/borrow_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class BorrowExprTests extends AbstractPassTest {

  "BorrowExprTests" should {

    "testBorrowExpr1" ignore AstFixture("""
        |func useString(_ str: String) {}
        |var global: String = "123"
        |func testGlobal() {
        |  useString(_borrow global)
        |}
        |""".stripMargin) { cpg => ??? }

    "testBorrowExpr2" ignore AstFixture("""
        |func useString(_ str: String) {}
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  useString(_borrow t)
        |}
        |""".stripMargin) { cpg => ??? }
  }

}
