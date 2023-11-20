

// This test file has been translated from swift/test/Parse/invalid_if_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class InvalidIfExprTests extends AbstractPassTest {
  "testInvalidIfExpr1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      (a ? b1️⃣)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"])
      ],
      fixedSource: "(a ? b : <#expression#>)"
    )
  }

  "testInvalidIfExpr2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      (a ? b : c ? d1️⃣)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"])
      ],
      fixedSource: "(a ? b : c ? d : <#expression#>)"
    )
  }

  "testInvalidIfExpr3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      ℹ️(a ? b ? c : d1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"]),
        DiagnosticSpec(
          message: "expected ')' to end tuple",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
      ],
      fixedSource: "(a ? b ? c : d : <#expression#>)"
    )
  }

  "testInvalidIfExpr4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      (a ? b ? c1️⃣)
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"]),
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"]),
      ],
      fixedSource: "(a ? b ? c : <#expression#> : <#expression#>)"
    )
  }

  "testInvalidIfExpr5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo ? 1 1️⃣2
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' after '? ...' in ternary expression", fixIts: ["insert ':'"])
      ],
      fixedSource: "foo ? 1 : 2"
    )
  }

  "testInvalidIfExpr6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo ? 1 1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"])
      ],
      fixedSource: "foo ? 1 : <#expression#>"
    )
  }

  "testInvalidIfExpr7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      condition ? 1 1️⃣
      someOtherVariable
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"])
      ],
      fixedSource: """
        condition ? 1 : <#expression#>
        someOtherVariable
        """
    )
  }
}
