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

class DoExpressionTests extends AbstractPassTest {
  override var experimentalFeatures: Parser.ExperimentalFeatures {
    [.doExpressions, .thenStatements]
  }

  "testDoExpr1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = do { 5 }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        }
      )
    )
  }

  "testDoExpr2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = do { 5 } catch { 0 }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        },
        catchClauses: CatchClauseListSyntax {
          CatchClauseSyntax {
            IntegerLiteralExprSyntax(0)
          }
        }
      )
    )
  }

  "testDoExpr3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() {
        do { 5 }
      }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        }
      )
    )
  }

  "testDoExpr4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() {
        do { 5 } as Int
      }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        }
      )
    )
  }

  "testDoExpr5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do { 5 } as Int
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        }
      )
    )
  }

  "testDoExpr6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() {
        do { 5 } catch { 0 } as Int
      }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        },
        catchClauses: CatchClauseListSyntax {
          CatchClauseSyntax {
            IntegerLiteralExprSyntax(0)
          }
        }
      )
    )
  }

  "testDoExpr7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      y = do { 5 } catch { 0 } as Int
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        },
        catchClauses: CatchClauseListSyntax {
          CatchClauseSyntax {
            IntegerLiteralExprSyntax(0)
          }
        }
      )
    )
  }

  "testDoExpr8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      { do { 5 } catch { 0 } }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(5)
        },
        catchClauses: CatchClauseListSyntax {
          CatchClauseSyntax {
            IntegerLiteralExprSyntax(0)
          }
        }
      )
    )
  }

  "testDoExpr9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        ()
        then 5
      }
      """,
      substructure: DoExprSyntax(
        doKeyword: .keyword(.do),
        body: CodeBlockSyntax {
          TupleExprSyntax(elements: .init())
          ThenStmtSyntax(expression: IntegerLiteralExprSyntax(5))
        }
      )
    )
  }

  "testDoExpr10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        0
        then 1
      } catch _ where true {
        2
      } catch let err as Err {
        3
        4
        then 5
      } catch {
        then 6
      }
      """,
      substructure: DoExprSyntax(
        body: CodeBlockSyntax {
          IntegerLiteralExprSyntax(0)
          ThenStmtSyntax(expression: IntegerLiteralExprSyntax(1))
        },
        catchClauses: CatchClauseListSyntax {
          CatchClauseSyntax(
            catchItems: [
              .init(
                pattern: ExpressionPatternSyntax(expression: DiscardAssignmentExprSyntax()),
                whereClause: WhereClauseSyntax(condition: BooleanLiteralExprSyntax(true))
              )
            ]
          ) {
            IntegerLiteralExprSyntax(2)
          }
          CatchClauseSyntax(
            catchItems: [
              CatchItemSyntax(
                pattern: ValueBindingPatternSyntax(
                  bindingSpecifier: .keyword(.let),
                  pattern: ExpressionPatternSyntax(
                    expression: SequenceExprSyntax {
                      PatternExprSyntax(
                        pattern: IdentifierPatternSyntax(identifier: .identifier("err"))
                      )
                      UnresolvedAsExprSyntax()
                      TypeExprSyntax(type: IdentifierTypeSyntax(name: "Err"))
                    }
                  )
                )
              )
            ]
          ) {
            IntegerLiteralExprSyntax(3)
            IntegerLiteralExprSyntax(4)
            ThenStmtSyntax(expression: IntegerLiteralExprSyntax(5))
          }
          CatchClauseSyntax {
            ThenStmtSyntax(expression: IntegerLiteralExprSyntax(6))
          }
        }
      )
    )
  }

  "testDoExpr11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return do { 5 }
      """,
      substructure:
        ReturnStmtSyntax(
          expression: DoExprSyntax(
            body: CodeBlockSyntax {
              IntegerLiteralExprSyntax(5)
            }
          )
        )
    )
  }

  "testDoExpr12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return
      do { 5 }
      """,
      substructure:
        ReturnStmtSyntax(
          expression: DoExprSyntax(
            body: CodeBlockSyntax {
              IntegerLiteralExprSyntax(5)
            }
          )
        )
    )
  }

  "testDoExprDisabled1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return1️⃣ do { 5 }
      """,
      substructure:
        DoStmtSyntax(
          body: CodeBlockSyntax {
            IntegerLiteralExprSyntax(5)
          }
        ),
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: [
            "insert newline", "insert ';'",
          ]
        )
      ],
      fixedSource: """
        return
        do { 5 }
        """,
      experimentalFeatures: []
    )
  }

  "testDoExprDisabled2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      return
      do { 5 }
      """,
      substructure:
        DoStmtSyntax(
          body: CodeBlockSyntax {
            IntegerLiteralExprSyntax(5)
          }
        ),
      experimentalFeatures: []
    )
  }

  "testDoExprDisabled3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do { 5 }
      """,
      substructure:
        DoStmtSyntax(
          body: CodeBlockSyntax {
            IntegerLiteralExprSyntax(5)
          }
        ),
      experimentalFeatures: []
    )
  }

  "testDoExprDisabled4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do { 5 } 1️⃣as Int
      """,
      substructure:
        DoStmtSyntax(
          body: CodeBlockSyntax {
            IntegerLiteralExprSyntax(5)
          }
        ),
      diagnostics: [
        DiagnosticSpec(message: "extraneous code 'as Int' at top level")
      ],
      experimentalFeatures: []
    )
  }

  "testDoExprDisabled5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let x = 1️⃣do { 5 }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected expression in variable",
          fixIts: ["insert expression"]
        )
      ],
      fixedSource: "let x = <#expression#>do { 5 }",
      experimentalFeatures: []
    )
  }
}
