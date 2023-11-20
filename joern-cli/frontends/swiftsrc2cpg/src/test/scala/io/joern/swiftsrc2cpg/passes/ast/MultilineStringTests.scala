

// This test file has been translated from swift/test/StringProcessing/Parse/multiline_string.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MultilineStringTests extends AbstractPassTest {
  "testMultilineString1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      import Swift
      """
    )
  }

  "testMultilineString2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // ===---------- Multiline --------===
      """
    )
  }

  "testMultilineString3" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          One
          ""Alpha""
          """
      """#
    )
  }

  "testMultilineString4" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Two
        Beta
        """
      """#
    )
  }

  "testMultilineString5" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Three
          Gamma.
        """
      """#
    )
  }

  "testMultilineString6" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Four
          Delta
      """
      """#
    )
  }

  "testMultilineString7" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Five\n

          Epsilon
          """
      """#
    )
  }

  "testMultilineString9" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Six
          Zeta

          """
      """#
    )
  }

  "testMultilineString11" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          Seven
          Eta\n
          """
      """#
    )
  }

  "testMultilineString12" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          \"""
          "\""
          ""\"
          Iota
          """
      """#
    )
  }

  "testMultilineString13" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
           \("Nine")
          Kappa
          """
      """#
    )
  }

  "testMultilineString14" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
      	first
      	 second
      	third
      	"""
      """#
    )
  }

  "testMultilineString15" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
       first
       	second
       third
       """
      """#
    )
  }

  "testMultilineString16" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
      \\
      """
      """#
    )
  }

  "testMultilineString17" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
        \\
        """
      """#
    )
  }

  "testMultilineString18" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

        ABC
        """
      """#
    )
  }

  "testMultilineString20" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

      ABC
      """
      """#
    )
  }

  "testMultilineString22" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

        ABC
        """
      """#
    )
  }

  "testMultilineString24" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // contains tabs
      _ = """
      	Twelve
      	Nu
      	"""
      """#
    )
  }

  "testMultilineString25" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
        newline \
        elided
        """
      """#
    )
  }

  "testMultilineString26" ignore AstFixture("") { cpg =>
    // contains trailing whitespace
    assertParse(
      #"""
      _ = """
        trailing \
        \("""
          substring1 \
          \("""
            substring2 \         \#u{20}
            substring3
            """)
          """) \
        whitespace
        """
      """#
    )
  }

  "testMultilineString27" ignore AstFixture("") { cpg =>
    // contains trailing whitespace
    assertParse(
      #"""
      _ = """
          foo

          bar
          """
      """#
    )
  }

  "testMultilineString29" ignore AstFixture("") { cpg =>
    // contains trailing whitespace
    assertParse(
      #"""
      _ = """
          foo\\#u{20}
         \#u{20}
          bar
          """
      """#
    )
  }

  "testMultilineString31" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          foo \
            bar
          """
      """#
    )
  }

  "testMultilineString32" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

        ABC
        """
      """#
    )
  }

  "testMultilineString34" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

          ABC

          """
      """#
    )
  }

  "testMultilineString37" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """


          """
      """#
    )
  }

  "testMultilineString39" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """

          """
      """#
    )
  }

  "testMultilineString41" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          """
      """#
    )
  }

  "testMultilineString42" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = "\("""
        \("a" + """
         valid
        """)
        """) literal"
      """#
    )
  }

  "testMultilineString43" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = "hello\("""
        world
        """)"
      """#
    )
  }

  "testMultilineString44" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
        hello\("""
           world
           """)
        abc
        """
      """#
    )
  }

  "testMultilineString45" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = "hello\("""
                  "world'
                  """)abc"
      """#
    )
  }

  "testMultilineString46" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """
          welcome
          \(
            /*
              ')' or '"""' in comment.
              """
            */
            "to\("""
                 Swift
                 """)"
            // ) or """ in comment.
          )
          !
          """
      """#
    )
  }

  "testMultilineString47" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """1️⃣"""
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "multi-line string literal closing delimiter must begin on a new line",
          fixIts: ["insert newline"]
        )
      ],
      fixedSource: #"""
        _ = """
        """
        """#
    )
  }

  "testMultilineString48" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = """A1️⃣"""
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "multi-line string literal closing delimiter must begin on a new line",
          fixIts: ["insert newline"]
        )
      ],
      fixedSource: #"""
        _ = """A
        """
        """#
    )
  }

  "testMultilineString49" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = ℹ️"""1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: #"expected '"""' to end string literal"#,
          notes: [NoteSpec(message: #"to match this opening '"""'"#)],
          fixIts: [#"insert '"""'"#]
        )
      ],
      fixedSource: #"""
        _ = """
        """
        """#
    )
  }

  "testMultilineString50" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = ℹ️"""
      A1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: #"expected '"""' to end string literal"#,
          notes: [NoteSpec(message: #"to match this opening '"""'"#)],
          fixIts: [#"insert '"""'"#]
        )
      ],
      fixedSource: #"""
        _ = """
        A
        """
        """#
    )
  }

  "testEscapeNewlineInRawString" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      #"""
      Three \#
      Gamma
      """#
      """##,
      substructure: StringLiteralExprSyntax(
        openingPounds: .rawStringPoundDelimiter("#"),
        openingQuote: .multilineStringQuoteToken(trailingTrivia: .newline),
        segments: StringLiteralSegmentListSyntax([
          .stringSegment(
            StringSegmentSyntax(content: .stringSegment("Three ", trailingTrivia: [.backslashes(1), .pounds(1), .newlines(1)]))
          ),
          .stringSegment(StringSegmentSyntax(content: .stringSegment("Gamma", trailingTrivia: .newline))),
        ]),
        closingQuote: .multilineStringQuoteToken(),
        closingPounds: .rawStringPoundDelimiter("#")
      ),
      options: .substructureCheckTrivia
    )
  }

  "testEscapeLastNewlineInRawString" ignore AstFixture("") { cpg =>
    // We really shouldn't be allow the last newline in the multi-line string
    // literal to be escaped in a multi-line string literal. But since the C++
    // parser accepts this, so does SwiftParser.
    assertParse(
      ##"""
      #"""
      Three \#
      Gamma \#
      """#
      """##,
      substructure: StringLiteralExprSyntax(
        openingPounds: .rawStringPoundDelimiter("#"),
        openingQuote: .multilineStringQuoteToken(trailingTrivia: .newline),
        segments: StringLiteralSegmentListSyntax([
          .stringSegment(
            StringSegmentSyntax(content: .stringSegment("Three ", trailingTrivia: [.backslashes(1), .pounds(1), .newlines(1)]))
          ),
          .stringSegment(StringSegmentSyntax(content: .stringSegment("Gamma ", trailingTrivia: [.backslashes(1), .pounds(1), .newlines(1)]))),
        ]),
        closingQuote: .multilineStringQuoteToken(),
        closingPounds: .rawStringPoundDelimiter("#")
      ),
      options: .substructureCheckTrivia
    )
  }
}
