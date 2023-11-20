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

@_spi(RawSyntax) @_spi(ExperimentalLanguageFeatures) import SwiftParser
@_spi(RawSyntax) @_spi(ExperimentalLanguageFeatures) import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ThenStatementTests extends AbstractPassTest {
  // Enable then statements by default.
  override var experimentalFeatures: Parser.ExperimentalFeatures {
    return .thenStatements
  }

  "testThenStmt1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then 0
      """,
      substructure: ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0))
    )
  }

  "testThenStmt2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then x
      """,
      substructure: ThenStmtSyntax(expression: DeclReferenceExprSyntax(baseName: "x"))
    )
  }

  "testThenStmt3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then ()
      """,
      substructure: ThenStmtSyntax(expression: TupleExprSyntax(elements: .init([])))
    )
  }

  "testThenStmt4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then (1)
      """,
      substructure:
        ThenStmtSyntax(
          expression: TupleExprSyntax(
            elements: .init([
              .init(expression: IntegerLiteralExprSyntax(1))
            ])
          )
        )
    )
  }

  "testThenStmt5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then (1, 2)
      """,
      substructure:
        ThenStmtSyntax(
          expression: TupleExprSyntax(
            elements: .init([
              .init(expression: IntegerLiteralExprSyntax(1), trailingComma: .commaToken()),
              .init(expression: IntegerLiteralExprSyntax(2)),
            ])
          )
        )
    )
  }

  "testThenStmt6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then ""
      """,
      substructure: ThenStmtSyntax(expression: StringLiteralExprSyntax(content: ""))
    )
  }

  "testThenStmt7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then []
      """,
      substructure: ThenStmtSyntax(expression: ArrayExprSyntax(elements: .init(expressions: [])))
    )
  }

  "testThenStmt8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then [0]
      """,
      substructure:
        ThenStmtSyntax(
          expression: ArrayExprSyntax(
            elements: .init(expressions: [
              .init(IntegerLiteralExprSyntax(0))
            ])
          )
        )
    )
  }

  "testThenStmt9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then if .random() { 0 } else { 1 }
      """
    )
  }

  "testThenStmt10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then -1
      """,
      substructure:
        ThenStmtSyntax(
          expression: PrefixOperatorExprSyntax(
            operator: .prefixOperator("-"),
            expression: IntegerLiteralExprSyntax(1)
          )
        )
    )
  }

  "testThenStmt11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then ~1
      """,
      substructure:
        ThenStmtSyntax(
          expression: PrefixOperatorExprSyntax(
            operator: .prefixOperator("~"),
            expression: IntegerLiteralExprSyntax(1)
          )
        )
    )
  }

  "testThenStmt12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then /.../
      """,
      substructure: ThenStmtSyntax(expression: RegexLiteralExprSyntax(regex: .regexLiteralPattern("...")))
    )
  }

  "testThenStmt13" ignore AstFixture("") { cpg =>
    // This is a a statement.
    assertParse(
      """
      then .foo
      """,
      substructure: ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo")))
    )
  }

  "testThenStmt14" ignore AstFixture("") { cpg =>
    // This is a member access.
    assertParse(
      """
      then.foo
      """,
      substructure:
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .identifier("then")),
          name: .identifier("foo")
        )
    )
  }

  "testThenStmt15" ignore AstFixture("") { cpg =>
    // This could be a member access too, but it seems rare enough to continue
    // parsing as a statement.
    assertParse(
      """
      then . foo
      """,
      substructure: ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo")))
    )
  }

  "testThenStmt16" ignore AstFixture("") { cpg =>
    // This will be diagnosed in ASTGen.
    assertParse(
      """
      a: then 0
      """,
      substructure:
        LabeledStmtSyntax(
          label: .identifier("a"),
          statement: ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0))
        )
    )
  }

  "testThenStmt17" ignore AstFixture("") { cpg =>
    // This is a function call.
    assertParse(
      """
      then()
      """,
      substructure:
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([]),
          rightParen: .rightParenToken()
        )
    )
  }

  "testThenStmt18" ignore AstFixture("") { cpg =>
    // This is a function call.
    assertParse(
      """
      then(0)
      """,
      substructure:
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([.init(expression: IntegerLiteralExprSyntax(0))]),
          rightParen: .rightParenToken()
        )
    )
  }

  "testThenStmt19" ignore AstFixture("") { cpg =>
    // This is a function call.
    assertParse(
      """
      then(x: 0)
      """,
      substructure:
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([.init(label: "x", expression: IntegerLiteralExprSyntax(0))]),
          rightParen: .rightParenToken()
        )
    )
  }

  "testThenStmt20" ignore AstFixture("") { cpg =>
    // This is a function call.
    assertParse(
      """
      then{}
      """,
      substructure:
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([]),
          trailingClosure: ClosureExprSyntax(statements: .init([]))
        )
    )
  }

  "testThenStmt21" ignore AstFixture("") { cpg =>
    // This is a function call.
    assertParse(
      """
      then {}
      """,
      substructure:
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([]),
          trailingClosure: ClosureExprSyntax(statements: .init([]))
        )
    )
  }

  "testThenStmt22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected expression in 'then' statement",
          fixIts: ["insert expression"]
        )
      ],
      fixedSource: "then <#expression#>"
    )
  }

  "testThenStmt23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then1️⃣;
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected expression in 'then' statement",
          fixIts: ["insert expression"]
        )
      ],
      fixedSource: "then <#expression#>;"
    )
  }

  "testThenStmt24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      (then)
      """,
      substructure:
        TupleExprSyntax(
          elements: .init([
            .init(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
          ])
        )
    )
  }

  "testThenStmt25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then
      0
      """,
      substructure: ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0))
    )
  }

  "testThenStmt26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = then
      """,
      substructure: DeclReferenceExprSyntax(baseName: .identifier("then"))
    )
  }

  "testThenStmt27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      self.then
      """,
      substructure:
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .keyword(.self)),
          name: .identifier("then")
        )
    )
  }

  "testThenStmt28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then + 2
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          BinaryOperatorExprSyntax(operator: .binaryOperator("+"))
          IntegerLiteralExprSyntax(2)
        }
    )
  }

  "testThenStmt29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then+2
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          BinaryOperatorExprSyntax(operator: .binaryOperator("+"))
          IntegerLiteralExprSyntax(2)
        }
    )
  }

  "testThenStmt30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then = 2
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          AssignmentExprSyntax()
          IntegerLiteralExprSyntax(2)
        }
    )
  }

  "testThenStmt31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then=2
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          AssignmentExprSyntax()
          IntegerLiteralExprSyntax(2)
        }
    )
  }

  "testThenStmt32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then is Int
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          UnresolvedIsExprSyntax()
          TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int")))
        }
    )
  }

  "testThenStmt33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then as Int
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          UnresolvedAsExprSyntax()
          TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int")))
        }
    )
  }

  "testThenStmt34" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then as? Int
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          UnresolvedAsExprSyntax(questionOrExclamationMark: .postfixQuestionMarkToken())
          TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int")))
        }
    )
  }

  "testThenStmt35" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then as! Int
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          UnresolvedAsExprSyntax(questionOrExclamationMark: .exclamationMarkToken())
          TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int")))
        }
    )
  }

  "testThenStmt36" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then ? 0 : 1
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          UnresolvedTernaryExprSyntax(thenExpression: IntegerLiteralExprSyntax(0))
          IntegerLiteralExprSyntax(1)
        }
    )
  }

  "testThenStmt37" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try then 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'try' must be placed on the produced expression",
          fixIts: ["move 'try' after 'then'"]
        )
      ],
      fixedSource: "then try 0"
    )
  }

  "testThenStmt38" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then try 0
      """
    )
  }

  "testThenStmt39" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then!
      """,
      substructure:
        ForceUnwrapExprSyntax(
          expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
        )
    )
  }

  "testThenStmt40" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then?
      """,
      substructure:
        OptionalChainingExprSyntax(
          expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
        )
    )
  }

  "testThenStmt41" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then?.foo
      """,
      substructure:
        MemberAccessExprSyntax(
          base: OptionalChainingExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          ),
          name: .identifier("foo")
        )
    )
  }

  "testThenStmt42" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then!.foo
      """,
      substructure:
        MemberAccessExprSyntax(
          base: ForceUnwrapExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          ),
          name: .identifier("foo")
        )
    )
  }

  "testThenStmt43" ignore AstFixture("") { cpg =>
    assertParse(
      """
      self.then(0)
      """,
      substructure:
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .keyword(.self)),
          name: .identifier("then")
        )
    )
  }

  "testThenStmt44" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then /^ then/
      """,
      substructure:
        SequenceExprSyntax {
          DeclReferenceExprSyntax(baseName: .identifier("then"))
          BinaryOperatorExprSyntax(operator: .binaryOperator("/^"))
          PostfixOperatorExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then")),
            operator: .postfixOperator("/")
          )
        }
    )
  }

  "testThenStmt45" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return then
      """,
      substructure:
        ReturnStmtSyntax(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
    )
  }

  "testThenStmt46" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then[0]
      """,
      substructure:
        SubscriptCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([
            .init(expression: IntegerLiteralExprSyntax(0))
          ])
        )
    )
  }

  "testThenStmt47" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then: for then in [] {
        break then
        continue then
      }
      """
    )
  }

  "testThenStmt48" ignore AstFixture("") { cpg =>
    assertParse(
      """
      throw then
      """,
      substructure:
        ThrowStmtSyntax(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
    )
  }

  "testThenStmt49" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try then()
      """
    )
  }

  "testThenStmt50" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try then{}
      """
    )
  }

  "testThenStmt51" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try then {}
      """
    )
  }

  "testThenStmt52" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try then + 1
      """
    )
  }

  "testThenStmt53" ignore AstFixture("") { cpg =>
    assertParse(
      """
      then
        .foo
      """,
      substructure: ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo")))
    )
  }

  "testThenStmt54" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return try then
      """,
      substructure:
        ReturnStmtSyntax(
          expression: TryExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          )
        )
    )
  }

  "testThenStmt55" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = [
        0,
        then
      ]
      """
    )
  }

  "testThenStmtDisabled1" ignore AstFixture("") { cpg =>
    // Make sure it's disabled by default.
    assertParse(
      """
      then1️⃣ 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: [
            "insert newline",
            "insert ';'",
          ]
        )
      ],
      fixedSource: """
        then
        0
        """,
      experimentalFeatures: []
    )
  }

  "testThenStmtDisabled2" ignore AstFixture("") { cpg =>
    // Make sure it's disabled by default. This is specifically testing
    // StmtSyntax.parse, since it will try to parse without checking
    // `atStartOfThenStatement`.
    assertParse(
      """
      1️⃣then 02️⃣
      """,
      { StmtSyntax.parse(from: &$0) },
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "unexpected code 'then 0' before statement"
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected statement",
          fixIts: ["insert statement"]
        ),
      ],
      fixedSource: "<#statement#>then 0",
      experimentalFeatures: []
    )
  }
}
