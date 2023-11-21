// This test file has been translated from swift/test/Parse/move_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MoveExprTests extends AbstractPassTest {

  "MoveExprTests" should {

    "testMoveExpr1" ignore AstFixture("""
        |var global: Int = 5
        |let _ = _move global
        |""".stripMargin) { cpg => ??? }

    "testConsumeExpr1" ignore AstFixture("""
        |var global: Int = 5
        |let _ = consume global
        |""".stripMargin) { cpg => ??? }

  }

}
