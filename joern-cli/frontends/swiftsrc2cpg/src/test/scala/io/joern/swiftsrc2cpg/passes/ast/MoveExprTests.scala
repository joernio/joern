

// This test file has been translated from swift/test/Parse/move_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MoveExprTests extends AbstractPassTest {
  "testMoveExpr1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var global: Int = 5
      "testGlobal" ignore AstFixture("") { cpg =>
          let _ = _move global
      }
      """
    )
  }

  "testMoveExpr2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testLet" ignore AstFixture("") { cpg =>
          let t = String()
          let _ = _move t
      }
      """
    )
  }

  "testMoveExpr3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testVar" ignore AstFixture("") { cpg =>
          var t = String()
          t = String()
          let _ = _move t
      }
      """
    )
  }

  "testConsumeExpr1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var global: Int = 5
      "testGlobal" ignore AstFixture("") { cpg =>
          let _ = consume global
      }
      """
    )
  }

  "testConsumeExpr2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testLet" ignore AstFixture("") { cpg =>
          let t = String()
          let _ = consume t
      }
      """
    )
  }

  "testConsumeExpr3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      "testVar" ignore AstFixture("") { cpg =>
          var t = String()
          t = String()
          let _ = consume t
      }
      """
    )
  }

  "testConsumeVariableNameInCast" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class ParentKlass {}
      class SubKlass : ParentKlass {}

      func test(_ x: SubKlass) {
        switch x {
        case let consume as ParentKlass:
          fallthrough
        }
      }
      """
    )
  }
}
