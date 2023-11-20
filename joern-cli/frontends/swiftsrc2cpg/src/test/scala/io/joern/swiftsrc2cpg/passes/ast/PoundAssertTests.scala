

// This test file has been translated from swift/test/Parse/pound_assert.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PoundAssertTests extends AbstractPassTest {
  "testPoundAssert1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #assert(true, 1️⃣123)
      """
    )
  }

  "testPoundAssert2" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #assert(true, "error \(1) message")
      """#
    )
  }

  "testPoundAssert3a" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #assert1️⃣ true2️⃣, "error message")
      """#,
      diagnostics: [
        DiagnosticSpec(message: "consecutive statements on a line must be separated by newline or ';'", fixIts: ["insert newline", "insert ';'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: #"extraneous code ', "error message")' at top level"#),
      ],
      applyFixIts: ["insert newline"],
      fixedSource: #"""
        #assert
        true, "error message")
        """#
    )
  }

  "testPoundAssert3b" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #assert1️⃣ true2️⃣, "error message")
      """#,
      diagnostics: [
        DiagnosticSpec(message: "consecutive statements on a line must be separated by newline or ';'", fixIts: ["insert newline", "insert ';'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: #"extraneous code ', "error message")' at top level"#),
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: #"""
        #assert; true, "error message")
        """#
    )
  }

  "testPoundAssert4" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #assert(1️⃣, "error message")
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected value in macro expansion", fixIts: ["insert value"])
      ],
      fixedSource: #"""
        #assert(<#expression#>, "error message")
        """#
    )
  }

  "testPoundAssert5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func unbalanced1() {
        #assertℹ️(true 1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' to end macro expansion",
          notes: [
            NoteSpec(message: "to match this opening '('")
          ],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: """
        func unbalanced1() {
          #assert(true)
        }
        """
    )
  }

  "testPoundAssert6" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      func unbalanced2() {
        #assertℹ️(true, "hello world" 1️⃣
      }
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' to end macro expansion",
          notes: [
            NoteSpec(message: "to match this opening '('")
          ],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: #"""
        func unbalanced2() {
          #assert(true, "hello world")
        }
        """#
    )
  }
}
