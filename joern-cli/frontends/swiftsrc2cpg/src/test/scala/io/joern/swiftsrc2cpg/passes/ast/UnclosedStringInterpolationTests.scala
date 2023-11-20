

// This test file has been translated from swift/test/Parse/unclosed-string-interpolation.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class UnclosedStringInterpolationTests extends AbstractPassTest {
  "testUnclosedStringInterpolation1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      let mid = "pete"
      """#
    )
  }

  "testUnclosedStringInterpolation2" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = "mid == \ℹ️(pete1️⃣"
      """##,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' in string literal",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: ##"""
        _ = "mid == \(pete)"
        """##
    )
  }

  "testUnclosedStringInterpolation3" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let theGoat = 1️⃣"kanye \2️⃣(3️⃣"4️⃣
      """##,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(locationMarker: "3️⃣", message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "expected ')' in string literal",
          notes: [NoteSpec(locationMarker: "2️⃣", message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(locationMarker: "1️⃣", message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
      ],
      fixedSource: ##"""
        let theGoat = "kanye \("")"
        """##
    )
  }

  "testUnclosedStringInterpolation4" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let equation1 = "2 + 2 = \ℹ️(2 + 21️⃣"
      """##,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' in string literal",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: ##"""
        let equation1 = "2 + 2 = \(2 + 2)"
        """##

    )
  }

  "testUnclosedStringInterpolation5" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let s = "\ℹ️(x1️⃣"; print(x)
      """##,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' in string literal",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: ##"""
        let s = "\(x)"; print(x)
        """##
    )
  }

  "testUnclosedStringInterpolation6" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let zzz = ℹ️"\(x1️⃣; print(x)2️⃣
      """##,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "unexpected code '; print(x' in string literal"
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
      ],
      fixedSource: ##"""
        let zzz = "\(x; print(x)"
        """##
    )
  }

  "testUnclosedStringInterpolation7" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let goatedAlbum = 1️⃣"The Life Of \2️⃣("Pablo"3️⃣
      """##,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "expected ')' in string literal",
          notes: [NoteSpec(locationMarker: "2️⃣", message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(locationMarker: "1️⃣", message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
      ],
      fixedSource: ##"""
        let goatedAlbum = "The Life Of \("Pablo")"
        """##
    )
  }

  "testUnclosedStringInterpolation8" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      _ = 1️⃣"""
      \2️⃣(
      3️⃣"""4️⃣
      """##,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"""' to end string literal"#,
          notes: [NoteSpec(locationMarker: "3️⃣", message: #"to match this opening '"""'"#)],
          fixIts: [#"insert '"""'"#]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "expected ')' in string literal",
          notes: [NoteSpec(locationMarker: "2️⃣", message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"""' to end string literal"#,
          notes: [NoteSpec(locationMarker: "1️⃣", message: #"to match this opening '"""'"#)],
          fixIts: [#"insert '"""'"#]
        ),
      ],
      fixedSource: ##"""
        _ = """
        \(
        """
        """)
        """
        """##
    )
  }

  "testSkipUnexpectedOpeningParensInStringLiteral" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️"\(e 1️⃣H()r2️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "unexpected code 'H(' in string literal"
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
      ],
      fixedSource: #"""
        "\(e H()r"
        """#
    )
  }

  "testUnterminatedStringLiteralInInterpolation" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      1️⃣"\2️⃣(3️⃣"4️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(locationMarker: "3️⃣", message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "expected ')' in string literal",
          notes: [NoteSpec(locationMarker: "2️⃣", message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: #"expected '"' to end string literal"#,
          notes: [NoteSpec(locationMarker: "1️⃣", message: #"to match this opening '"'"#)],
          fixIts: [#"insert '"'"#]
        ),
      ],
      fixedSource: #"""
        "\("")"
        """#
    )
  }
}
