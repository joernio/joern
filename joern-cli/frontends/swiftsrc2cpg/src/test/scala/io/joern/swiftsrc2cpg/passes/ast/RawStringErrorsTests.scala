

// This test file has been translated from swift/test/Parse/raw_string_errors.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class RawStringErrorsTests extends AbstractPassTest {
  "testRawStringErrors1" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      let _ = "foo\(#"bar"#1️⃣#)baz"
      """###,
      diagnostics: [
        DiagnosticSpec(message: "too many '#' characters in closing delimiter", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: ###"""
        let _ = "foo\(#"bar"#)baz"
        """###
    )
  }

  "testRawStringErrors2" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      let _ = #"\#1️⃣#("invalid")"#
      """###,
      diagnostics: [
        DiagnosticSpec(message: "too many '#' characters to start string interpolation", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: ###"""
        let _ = #"\#("invalid")"#
        """###
    )
  }

  "testRawStringErrors3" ignore AstFixture("") { cpg =>
    assertParse(
      #####"""
      let _ = ###"""invalid"###1️⃣###
      """#####,
      diagnostics: [
        DiagnosticSpec(message: "too many '#' characters in closing delimiter", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: #####"""
        let _ = ###"""invalid"###
        """#####
    )
  }

  "testRawStringErrors4" ignore AstFixture("") { cpg =>
    assertParse(
      #####"""
      let _ = ####"invalid"###1️⃣
      """#####,
      diagnostics: [
        DiagnosticSpec(message: #####"expected '"####' to end string literal"#####, fixIts: [#####"insert '"####'"#####])
      ],
      fixedSource: #####"""
        let _ = ####"invalid"###"####
        """#####
    )
  }

  "testRawStringErrors5" ignore AstFixture("") { cpg =>
    assertParse(
      #####"""
      let _ = ###"invalid"###1️⃣###
      """#####,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "too many '#' characters in closing delimiter", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: #####"""
        let _ = ###"invalid"###
        """#####
    )
  }

  "testRawStringErrors6" ignore AstFixture("") { cpg =>
    assertParse(
      ###"""
      let _ = ##"""1️⃣aa
        foobar
        aa2️⃣"""##
      """###,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "multi-line string literal content must begin on a new line", fixIts: ["insert newline"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "multi-line string literal closing delimiter must begin on a new line", fixIts: ["insert newline"]),
      ],
      fixedSource: ###"""
        let _ = ##"""
          aa
          foobar
          aa
          """##
        """###
    )
  }

  "testRawStringErrors7" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      let _ = #"""1️⃣ foo "bar" #baz
        """#
      """##,
      diagnostics: [
        DiagnosticSpec(message: "multi-line string literal content must begin on a new line", fixIts: ["insert newline"])
      ],
      fixedSource: ##"""
        let _ = #"""
           foo "bar" #baz
          """#
        """##
    )
  }

  "testRawStringErrors8" ignore AstFixture("") { cpg =>
    assertParse(
      ####"""
      let _ = ###"""1️⃣ "# "##
        """###
      """####,
      diagnostics: [
        DiagnosticSpec(message: "multi-line string literal content must begin on a new line", fixIts: ["insert newline"])
      ],
      fixedSource: ####"""
        let _ = ###"""
           "# "##
          """###
        """####
    )
  }

}
