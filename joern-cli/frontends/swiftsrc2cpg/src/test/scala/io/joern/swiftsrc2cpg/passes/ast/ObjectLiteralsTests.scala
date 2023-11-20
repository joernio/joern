

// This test file has been translated from swift/test/Parse/object_literals.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ObjectLiteralsTests extends AbstractPassTest {
  "testObjectLiterals1a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [#Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)1️⃣#2️⃣]
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ',' in array element", fixIts: ["insert ','"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        let _ = [#Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha), #<#identifier#>]
        """
    )
  }

  "testObjectLiterals1b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [#Image(imageLiteral: localResourceNameAsString)1️⃣#2️⃣]
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ',' in array element", fixIts: ["insert ','"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        let _ = [#Image(imageLiteral: localResourceNameAsString), #<#identifier#>]
        """
    )
  }

  "testObjectLiterals1c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [#FileReference(fileReferenceLiteral: localResourceNameAsString)1️⃣#2️⃣]
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ',' in array element", fixIts: ["insert ','"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        let _ = [#FileReference(fileReferenceLiteral: localResourceNameAsString), #<#identifier#>]
        """
    )
  }

  "testObjectLiterals2a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #Color(colorLiteralRed: red, green: green, blue: blue, alpha: alpha)
      """
    )
  }

  "testObjectLiterals2b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #Image(imageLiteral: localResourceNameAsString)
      """
    )
  }

  "testObjectLiterals2c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #FileReference(fileReferenceLiteral: localResourceNameAsString)
      """
    )
  }

  "testObjectLiterals3a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #notAPound
      """
    )
  }

  "testObjectLiterals3b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #notAPound(1, 2)
      """
    )
  }

  "testObjectLiterals3c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #Color
      """
    )
  }

  "testObjectLiterals4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [#1️⃣#2️⃣]
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ',' in array element", fixIts: ["insert ','"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        let _ = [#<#identifier#>, #<#identifier#>]
        """
    )
  }

  "testObjectLiterals5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = ℹ️[#Color(_: 1, green: 1, 2)1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected ']' to end array",
          notes: [NoteSpec(message: "to match this opening '['")],
          fixIts: ["insert ']'"]
        )
      ],
      fixedSource: """
        let _ = [#Color(_: 1, green: 1, 2)]
        """
    )
  }

  "testObjectLiterals6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = ℹ️[#Color(red: 1, green: 1, blue: 1)1️⃣#2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected ',' in array element",
          fixIts: ["insert ','"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected identifier in macro expansion",
          fixIts: ["insert identifier"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected ']' to end array",
          notes: [NoteSpec(message: "to match this opening '['")],
          fixIts: ["insert ']'"]
        ),
      ],
      fixedSource: """
        let _ = [#Color(red: 1, green: 1, blue: 1), #<#identifier#>]
        """
    )
  }

  "testObjectLiterals7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = [#Color(withRed: 1, green: 1, whatever: 2)1️⃣#2️⃣]
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected ',' in array element", fixIts: ["insert ','"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        let _ = [#Color(withRed: 1, green: 1, whatever: 2), #<#identifier#>]
        """
    )
  }

  "testObjectLiterals8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = #Color(_: 1, green: 1)
      """
    )
  }
}
