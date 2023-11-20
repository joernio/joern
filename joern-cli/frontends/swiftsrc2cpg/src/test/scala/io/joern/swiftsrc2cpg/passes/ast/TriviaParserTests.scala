package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TriviaParserTests extends AbstractPassTest {

  "testTriviaParsing" ignore AstFixture("") { cpg =>

    XCTAssertEqual(
      TriviaParser.parseTrivia(
        """
        /* */
        /**/
        /* /** */ */
        /** /* */ */
        """,
        position: .leading
      ),
      [
        .blockComment("/* */"),
        .newlines(1),
        .blockComment("/**/"),
        .newlines(1),
        .blockComment("/* /** */ */"),
        .newlines(1),
        .docBlockComment("/** /* */ */"),
      ]
    )

    XCTAssertEqual(
      TriviaParser.parseTrivia(
        """
        #!/bin/env swift
        """,
        position: .trailing
      ),
      [
        .pounds(1),
        .unexpectedText("!/bin/env"),
        .spaces(1),
        .unexpectedText("swift"),
      ]
    )

    XCTAssertEqual(
      TriviaParser.parseTrivia("\r\n\r\r\n\r\n\n", position: .leading),
      [
        .carriageReturnLineFeeds(1),
        .carriageReturns(1),
        .carriageReturnLineFeeds(2),
        .newlines(1),
      ]
    )

    let bom: Unicode.Scalar = "\u{feff}"
    var bomStr = "\(bom)/// Foo\n"
    bomStr.withSyntaxText { triviaText in
      XCTAssertEqual(
        TriviaParser.parseTrivia(triviaText, position: .leading),
        [
          .unexpectedText("\u{feff}"),
          .docLineComment("/// Foo"),
          .newlines(1),
        ]
      )
    }

    XCTAssertEqual(
      TriviaParser.parseTrivia(
        """
        // diff3-style conflict markers

        <<<<<<< HEAD:conflict_markers.swift // expected-error {{source control conflict marker in source file}}
        var a : String = "A"
        var b : String = "b"
        =======
        var a : String = "a"
        var b : String = "B"
        >>>>>>> 18844bc65229786b96b89a9fc7739c0fc897905e:conflict_markers.swift

        """,
        position: .leading
      ),
      [
        .lineComment("// diff3-style conflict markers"),
        .newlines(2),
        .unexpectedText(
          """
          <<<<<<< HEAD:conflict_markers.swift // expected-error {{source control conflict marker in source file}}
          var a : String = "A"
          var b : String = "b"
          =======
          var a : String = "a"
          var b : String = "B"
          >>>>>>> 18844bc65229786b96b89a9fc7739c0fc897905e:conflict_markers.swift
          """
        ),
        .newlines(1),
      ]
    )

    XCTAssertEqual(
      TriviaParser.parseTrivia(
        """
        // Perforce-style conflict markers

        >>>> ORIGINAL
        var a : String = "A"
        var b : String = "B"
        ==== THEIRS
        var a : String = "A"
        var b : String = "b"
        ==== YOURS
        var a : String = "a"
        var b : String = "B"
        <<<<

        """,
        position: .leading
      ),
      [
        .lineComment("// Perforce-style conflict markers"),
        .newlines(2),
        .unexpectedText(
          """
          >>>> ORIGINAL
          var a : String = "A"
          var b : String = "B"
          ==== THEIRS
          var a : String = "A"
          var b : String = "b"
          ==== YOURS
          var a : String = "a"
          var b : String = "B"
          <<<<

          """
        ),
      ]
    )
  }

  "testRawSyntaxLazyTriviaPieces" ignore AstFixture("") { cpg =>
    withParser(
      source: """
        /// Foo.
        func foo() {
        }
        """
    ) { parser in
      let fn = DeclSyntax.parse(from: &parser).cast(FunctionDeclSyntax.self)

      XCTAssertEqual(
        fn.funcKeyword.leadingTrivia,
        [
          .docLineComment("/// Foo."),
          .newlines(1),
        ]
      )
      XCTAssertEqual(
        fn.funcKeyword.trailingTrivia,
        [
          .spaces(1)
        ]
      )

      XCTAssertEqual(fn.body!.leftBrace.leadingTrivia, [])
      XCTAssertEqual(fn.body!.leftBrace.trailingTrivia, [])

      XCTAssertEqual(
        fn.body!.rightBrace.leadingTrivia,
        [
          .newlines(1)
        ]
      )
      XCTAssertEqual(fn.body!.rightBrace.trailingTrivia, [])
    }

  }

  "testSyntaxLazyTrivia" ignore AstFixture("") { cpg =>
    let source = """
      /* comment only */

      """
    let sourceFileSyntax = Parser.parse(source: source)
    XCTAssertEqual(
      sourceFileSyntax.leadingTrivia,
      [
        .blockComment("/* comment only */"),
        .newlines(1),
      ]
    )
  }

  "testUnexpectedSplitting" ignore AstFixture("") { cpg =>
    XCTAssertEqual(
      TriviaParser.parseTrivia("\u{fffe} ", position: .trailing),
      [
        .unexpectedText("\u{fffe}"),
        .spaces(1),
      ]
    )
  }
}
