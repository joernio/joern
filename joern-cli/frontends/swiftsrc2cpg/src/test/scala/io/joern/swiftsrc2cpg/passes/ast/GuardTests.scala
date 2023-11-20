

// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GuardTests extends AbstractPassTest {
  "testGuard1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func noConditionNoElse() {
        guard {} 1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'else' and body in 'guard' statement", fixIts: ["insert 'else' and body"])
      ],
      fixedSource: """
        func noConditionNoElse() {
          guard {} else {
        }
        }
        """
    )
  }

  "testGuard2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func noCondition() {
        guard 1️⃣else {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected condition in 'guard' statement", fixIts: ["insert condition"])
      ],
      fixedSource: """
        func noCondition() {
          guard <#expression#> else {}
        }
        """
    )
  }
}
