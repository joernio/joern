//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(RawSyntax) import SwiftParser
@_spi(RawSyntax) import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class RegexLiteralTests extends AbstractPassTest {
  "testForwardSlash1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      /(?<identifier>[[:alpha:]]\w*) = (?<hex>[0-9A-F]+)/
      """#
    )
  }

  "testForwardSlash2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      postfix func /(lhs: Int) -> Int {1/}
      """
    )
  }

  "testEmpty" ignore AstFixture("") { cpg =>
    assertParse("#//#")
  }

  "testExtraneous1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #//#1️⃣#
      """#,
      diagnostics: [
        DiagnosticSpec(message: "too many '#' characters in closing delimiter", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: "#//#"
    )
  }
  "testExtraneous2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/abc/#1️⃣#
      """,
      diagnostics: [
        DiagnosticSpec(message: "too many '#' characters in closing delimiter", fixIts: ["remove extraneous delimiters"])
      ],
      fixedSource: "#/abc/#"
    )
  }

  "testUnterminated1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in macro expansion", fixIts: ["insert identifier"])
      ],
      fixedSource: #"""
        #<#identifier#>
        """#
    )
  }

  "testUnterminated2" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️/1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "expected '/' to end regex literal",
          notes: [NoteSpec(message: "to match this opening '/'")],
          fixIts: ["insert '/'"]
        )
      ],
      fixedSource: #"""
        //
        """#
    )
  }

  "testUnterminated3" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      /#1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in macro expansion", fixIts: ["insert identifier"])
      ],
      fixedSource: #"""
        /#<#identifier#>
        """#
    )
  }

  "testUnterminated4" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"])
      ],
      fixedSource: #"""
        #//#
        """#
    )
  }

  "testUnterminated5" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️#//1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "expected '#' to end regex literal",
          notes: [NoteSpec(message: "to match this opening '#'")],
          fixIts: ["insert '#'"]
        )
      ],
      fixedSource: #"""
        #//#
        """#
    )
  }

  "testUnterminated6" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️#///1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "expected '#' to end regex literal",
          notes: [NoteSpec(message: "to match this opening '#'")],
          fixIts: ["insert '#'"]
        )
      ],
      fixedSource: #"""
        #///#
        """#
    )
  }

  "testUnterminated7" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️#/#1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"])
      ],
      fixedSource: #"""
        #/#/#
        """#
    )
  }

  "testUnterminated8" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/##1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"])
      ],
      fixedSource: #"""
        #/##/#
        """#
    )
  }

  "testUnterminated9" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ℹ️#/##/1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(
          message: "expected '#' to end regex literal",
          notes: [NoteSpec(message: "to match this opening '#'")],
          fixIts: ["insert '#'"]
        )
      ],
      fixedSource: #"""
        #/##/#
        """#
    )
  }

  "testUnterminated10" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ##/##/#1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected additional '#' characters in closing delimiter", fixIts: ["insert additional closing '#' delimiters"])
      ],
      fixedSource: "##/##/##"
    )
  }

  "testUnterminated11" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ##/###1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/##' to end regex literal", fixIts: ["insert '/##'"])
      ],
      fixedSource: "##/###/##"
    )
  }

  "testUnterminated12" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/\/#1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"])
      ],
      fixedSource: #"#/\/#/#"#
    )
  }

  "testUnterminated13" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ##/abc/#def1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/##' to end regex literal", fixIts: ["insert '/##'"])
      ],
      fixedSource: "##/abc/#def/##"
    )
  }

  "testUnterminated14" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ##/abc/def#1️⃣
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected '/##' to end regex literal", fixIts: ["insert '/##'"])
      ],
      fixedSource: "##/abc/def#/##"
    )
  }

  "testTerminated1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #//#
      """#
    )
  }

  "testTerminated2" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #///#
      """#
    )
  }

  "testTerminated3" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/#//#
      """#
    )
  }

  "testTerminated4" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      ##/##/##
      """#
    )
  }

  "testTerminated5" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/\/#/#
      """#
    )
  }

  "testTerminated6" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/\//#
      """#
    )
  }

  "testTerminated7" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      #/\\/#
      """#
    )
  }

  "testUnprintable1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /1️⃣\u{7F}/
      """,
      diagnostics: [
        DiagnosticSpec(message: "unprintable ASCII character found in source file")
      ]
    )
  }

  "testUnprintable2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/1️⃣\u{7F}/#
      """,
      diagnostics: [
        DiagnosticSpec(message: "unprintable ASCII character found in source file")
      ]
    )
  }

  "testMultiline1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/
      abc1️⃣/#
      """,
      diagnostics: [
        DiagnosticSpec(message: "multi-line regex closing delimiter must appear on new line")
      ]
    )
  }

  "testMultiline2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/abc1️⃣
      /#2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        #/abc/#
        /#<#identifier#>
        """
    )
  }

  "testMultiline3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/abc1️⃣
          \t \t /#2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '/#' to end regex literal", fixIts: ["insert '/#'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
      ],
      fixedSource: """
        #/abc/#
            \t \t /#<#identifier#>
        """
    )
  }

  "testMultiline4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/
       abc
          \t \t /#
      """
    )
  }

  "testMultiline5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/
      #1️⃣/#
      """,
      diagnostics: [
        DiagnosticSpec(message: "multi-line regex closing delimiter must appear on new line")
      ]
    )
  }

  "testOpeningSpace1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /1️⃣ a/
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningSpace2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = /1️⃣ a/
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningSpace3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/ a/#
      """
    )
  }

  "testClosingSpace1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /a /1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression after operator", fixIts: ["insert expression"])
      ],
      fixedSource: """
        /a / <#expression#>
        """
    )
  }

  "testClosingSpace2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = /a /1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression after operator", fixIts: ["insert expression"])
      ],
      fixedSource: """
        let x = /a / <#expression#>
        """
    )
  }

  "testClosingSpace3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/a /#
      """
    )
  }

  "testOpeningAndClosingSpace1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /1️⃣  /
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningAndClosingSpace2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x += /1️⃣  /
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningAndClosingSpace3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/  /#
      """
    )
  }

  "testOpeningAndClosingSpace4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /1️⃣ /
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningAndClosingSpace5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = /1️⃣ /
      """,
      diagnostics: [
        DiagnosticSpec(message: "bare slash regex literal may not start with space")
      ]
    )
  }

  "testOpeningAndClosingSpace6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #/ /#
      """
    )
  }

  "testSingleLineTabChar" ignore AstFixture("") { cpg =>
    // We currently only keep track of one lexer error, so only diagnose the first.
    assertParse(
      """
      #/1️⃣\t\t/#
      """,
      diagnostics: [
        DiagnosticSpec(message: "unprintable ASCII character found in source file")
      ]
    )
  }

  "testBinOpDisambiguation1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x /^ y ^/ z
      """
    )
  }

  "testBinOpDisambiguation2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x /^ y/
      """
    )
  }

  "testBinOpDisambiguation3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x !/^ y/
      """
    )
  }

  "testBinOpDisambiguation4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x !/^ y !/ z
      """
    )
  }

  "testBinOpDisambiguation5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try? /^ x/
      """
    )
  }

  "testBinOpDisambiguation6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try? /^ x ^/
      """
    )
  }

  "testBinOpDisambiguation7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try! /^ x/
      """
    )
  }

  "testBinOpDisambiguation8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try? /^ x ^/
      """
    )
  }

  "testBinOpDisambiguation9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x < /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      { /^ }}x/ }
      """
    )
  }

  "testBinOpDisambiguation11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      ( /^ }}x/ )
      """
    )
  }

  "testBinOpDisambiguation12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      [ /^ }}x/ ]
      """
    )
  }

  "testBinOpDisambiguation13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      foo(&/^ }}x/)
      """
    )
  }

  "testBinOpDisambiguation14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x; /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      [0 : /^ }}x/]
      """
    )
  }

  "testBinOpDisambiguation16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      (0, /^ }}x/)
      """
    )
  }

  "testBinOpDisambiguation17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x^ /^ x/
      """
    )
  }

  "testBinOpDisambiguation18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x! /^ x/
      """
    )
  }

  "testBinOpDisambiguation19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x? /^ x/
      """
    )
  }

  "testBinOpDisambiguation20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x > /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      {} /^ x/
      """
    )
  }

  "testBinOpDisambiguation22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      () /^ x/
      """
    )
  }

  "testBinOpDisambiguation23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      [] /^ x/
      """
    )
  }

  "testBinOpDisambiguation24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x... /^ x/
      """
    )
  }

  "testBinOpDisambiguation25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x.1️⃣ /^ x/
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected name in member access", fixIts: ["insert name"])
      ],
      fixedSource: """
        x.<#identifier#> /^ x/
        """
    )
  }

  "testBinOpDisambiguation26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #1️⃣ /^ x/
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in macro expansion", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        #<#identifier#> /^ x/
        """
    )
  }

  "testBinOpDisambiguation27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      `x` /^ x/
      """
    )
  }

  "testBinOpDisambiguation28" ignore AstFixture("") { cpg =>
    // FIXME: The diagnostic should be one character back
    assertParse(
      #"""
      \ 1️⃣/^ x/
      """#,
      diagnostics: [
        DiagnosticSpec(message: "expected root in key path", fixIts: ["insert root"])
      ],
      fixedSource: #"""
        \<#type#> /^ x/
        """#
    )
  }

  "testBinOpDisambiguation29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x /^ x/
      """
    )
  }

  "testBinOpDisambiguation30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      true /^ x/
      """
    )
  }

  "testBinOpDisambiguation31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      false /^ x/
      """
    )
  }

  "testBinOpDisambiguation32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x as Any /^ x/
      """
    )
  }

  "testBinOpDisambiguation34" ignore AstFixture("") { cpg =>
    assertParse(
      """
      nil /^ x/
      """
    )
  }

  "testBinOpDisambiguation35" ignore AstFixture("") { cpg =>
    assertParse(
      """
      .none /^ x/
      """
    )
  }

  "testBinOpDisambiguation36" ignore AstFixture("") { cpg =>
    assertParse(
      """
      .objc /^ x/
      """
    )
  }

  "testBinOpDisambiguation37" ignore AstFixture("") { cpg =>
    assertParse(
      """
      P.Protocol /^ x/
      """
    )
  }

  "testBinOpDisambiguation38" ignore AstFixture("") { cpg =>
    assertParse(
      """
      self /^ x/
      """
    )
  }

  "testBinOpDisambiguation39" ignore AstFixture("") { cpg =>
    assertParse(
      """
      Self /^ x/
      """
    )
  }

  "testBinOpDisambiguation40" ignore AstFixture("") { cpg =>
    assertParse(
      """
      super /^ x/
      """
    )
  }

  "testBinOpDisambiguation41" ignore AstFixture("") { cpg =>
    // await is a contextual keyword, so we can't assume it must be a regex.
    assertParse(
      """
      await 1️⃣/^ x/
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression in 'await' expression", fixIts: ["insert expression"])
      ],
      fixedSource: """
        await <#expression#> /^ x/
        """
    )
  }

  "testBinOpDisambiguation42" ignore AstFixture("") { cpg =>
    // await is a contextual keyword, so we can't assume it must be a regex.
    assertParse(
      """
      ^await /^ x/
      """
    )
  }

  "testBinOpDisambiguation43" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x ? /^ }}x/ : /x/
      """
    )
  }

  "testBinOpDisambiguation44" ignore AstFixture("") { cpg =>
    assertParse(
      """
      x ? /x/ : /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation45" ignore AstFixture("") { cpg =>
    assertParse(
      """
      0 /^ x/
      """
    )
  }

  "testBinOpDisambiguation46" ignore AstFixture("") { cpg =>
    assertParse(
      """
      0.1 /^ x/
      """
    )
  }

  "testBinOpDisambiguation47" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #if /^ }}x/
      #endif
      """
    )
  }

  "testBinOpDisambiguation48" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #if true
      #else
      /^ }}x/
      #endif
      """
    )
  }

  "testBinOpDisambiguation49" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #if true
      #elseif /^ }}x/
      #endif
      """
    )
  }

  "testBinOpDisambiguation50" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #if true
      #endif
      /^ }}x/
      """
    )
  }

  "testBinOpDisambiguation51" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      foo(a: /, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          label: "a",
          colon: .colonToken(),
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation52" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      foo(a, /, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: "a"),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation53" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      foo(a, ^/, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: "a"),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("^/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation54" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      foo(a: ^/, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          label: "a",
          colon: .colonToken(),
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("^/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation55" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      foo(^/, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("^/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation56" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      (^/, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("^/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation57" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      (/, /)
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation58" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      x[/, /]
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation59" ignore AstFixture("") { cpg =>
    // Unapplied operators, not regex.
    assertParse(
      """
      x[^/, /]
      """,
      substructure: LabeledExprListSyntax([
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("^/")),
          trailingComma: .commaToken()
        ),
        .init(
          expression: DeclReferenceExprSyntax(baseName: .binaryOperator("/"))
        ),
      ])
    )
  }

  "testBinOpDisambiguation60" ignore AstFixture("") { cpg =>
    // Invalid. We can't confidently lex as a regex (as the lexer thinks it
    // could be a subscript), so we get a parser error.
    assertParse(
      """
      [1️⃣/, /]
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '/, /' in array")
      ]
    )
  }

  "testBinOpDisambiguation61" ignore AstFixture("") { cpg =>
    // Fine if there's no trailing space though.
    assertParse(
      """
      [/,/]
      """,
      substructure: RegexLiteralExprSyntax(regex: .regexLiteralPattern(","))
    )
  }

  "testPrefixOpSplitting1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x =1️⃣/abc/
      """,
      diagnostics: [
        DiagnosticSpec(message: "'=' must have consistent whitespace on both sides", fixIts: ["insert whitespace"])
      ],
      fixedSource: """
        let x = /abc/
        """
    )
  }

  "testPrefixOpSplitting2a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x 1️⃣.2️⃣/abc/
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected '=' in variable",
          fixIts: ["insert '='"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected name in member access",
          fixIts: ["insert name"]
        ),
      ],
      applyFixIts: ["insert '='", "insert name"],
      fixedSource: """
        let x = .<#identifier#>/abc/
        """
    )
  }

  "testPrefixOpSplitting2b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x 1️⃣.2️⃣/abc/
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected '=' in variable",
          fixIts: ["insert '='"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected name in member access",
          fixIts: ["insert name"]
        ),
      ],
      applyFixIts: ["insert '='", "insert name"],
      fixedSource: """
        let x = .<#identifier#>/abc/
        """
    )
  }

  "testPrefixOpSplitting3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = true?/abc/1️⃣:/def/
      """,
      substructure: BinaryOperatorExprSyntax(operator: .binaryOperator("/")),
      diagnostics: [
        DiagnosticSpec(message: "extraneous code ':/def/' at top level")
      ]
    )
  }

  "testPrefixOpSplitting4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = true ?/abc/ : /def/
      """,
      substructure: SequenceExprSyntax {
        BooleanLiteralExprSyntax(booleanLiteral: true)
        UnresolvedTernaryExprSyntax(thenExpression: RegexLiteralExprSyntax(regex: .regexLiteralPattern("abc")))
        RegexLiteralExprSyntax(regex: .regexLiteralPattern("def"))
      }
    )
  }

  "testPrefixOpSplitting5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = &/abc/
      """,
      substructure: InOutExprSyntax(
        expression: RegexLiteralExprSyntax(
          regex: .regexLiteralPattern("abc")
        )
      )
    )
  }

  "testNulCharacter" ignore AstFixture("") { cpg =>
    assertParse(
      "/1️⃣\0/",
      diagnostics: [
        DiagnosticSpec(message: "nul character embedded in middle of file", severity: .warning)
      ]
    )
  }

  "testEmoji" ignore AstFixture("") { cpg =>
    assertParse("/👍/")
  }
}
