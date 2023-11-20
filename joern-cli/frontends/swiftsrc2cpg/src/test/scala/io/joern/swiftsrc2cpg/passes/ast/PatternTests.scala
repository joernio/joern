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

class PatternTests extends AbstractPassTest {
  private var genericArgEnumPattern: Syntax {
    // let E<Int>.e(y)
    Syntax(
      ValueBindingPatternSyntax(
        bindingSpecifier: .keyword(.let),
        pattern: ExpressionPatternSyntax(
          expression: FunctionCallExprSyntax(
            calledExpression: MemberAccessExprSyntax(
              base: GenericSpecializationExprSyntax(
                expression: DeclReferenceExprSyntax(baseName: .identifier("E")),
                genericArgumentClause: GenericArgumentClauseSyntax(
                  arguments: .init([
                    .init(argument: IdentifierTypeSyntax(name: .identifier("Int")))
                  ])
                )
              ),
              name: .identifier("e")
            ),
            leftParen: .leftParenToken(),
            arguments: LabeledExprListSyntax([
              .init(
                expression: PatternExprSyntax(
                  pattern: IdentifierPatternSyntax(identifier: .identifier("y"))
                )
              )
            ]),
            rightParen: .rightParenToken()
          )
        )
      )
    )
  }

  "testNonBinding1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case let E<Int>.e(y) = x {}
      """,
      substructure: genericArgEnumPattern
    )
  }

  "testNonBinding2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch e {
      case let E<Int>.e(y):
        y
      }
      """,
      substructure: genericArgEnumPattern
    )
  }

  private var tupleWithSubscriptAndBindingPattern: Syntax {
    // let (y[0], z)
    Syntax(
      ValueBindingPatternSyntax(
        bindingSpecifier: .keyword(.let),
        pattern: ExpressionPatternSyntax(
          expression: TupleExprSyntax(
            elements: .init([
              .init(
                expression: SubscriptCallExprSyntax(
                  calledExpression: DeclReferenceExprSyntax(baseName: .identifier("y")),
                  leftSquare: .leftSquareToken(),
                  arguments: LabeledExprListSyntax([
                    .init(expression: IntegerLiteralExprSyntax(literal: .integerLiteral("0")))
                  ]),
                  rightSquare: .rightSquareToken()
                ),
                trailingComma: .commaToken()
              ),
              .init(
                expression: PatternExprSyntax(
                  pattern: IdentifierPatternSyntax(identifier: .identifier("z"))
                )
              ),
            ])
          )
        )
      )
    )
  }

  "testNonBinding3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case let (y[0], z) = x {}
      """,
      substructure: tupleWithSubscriptAndBindingPattern
    )
  }

  "testNonBinding4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch x {
      case let (y[0], z):
        z
      }
      """,
      substructure: tupleWithSubscriptAndBindingPattern
    )
  }

  private var subscriptWithBindingPattern: Syntax {
    // let y[z]
    Syntax(
      ValueBindingPatternSyntax(
        bindingSpecifier: .keyword(.let),
        pattern: ExpressionPatternSyntax(
          expression: SubscriptCallExprSyntax(
            calledExpression: DeclReferenceExprSyntax(baseName: .identifier("y")),
            leftSquare: .leftSquareToken(),
            arguments: LabeledExprListSyntax([
              .init(
                expression: PatternExprSyntax(
                  pattern: IdentifierPatternSyntax(identifier: .identifier("z"))
                )
              )
            ]),
            rightSquare: .rightSquareToken()
          )
        )
      )
    )
  }

  "testNonBinding5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case let y[z] = x {}
      """,
      substructure: subscriptWithBindingPattern
    )
  }

  "testNonBinding6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch 0 {
      case let y[z]:
        z
      case y[z]:
        0
      default:
        0
      }
      """,
      substructure: subscriptWithBindingPattern
    )
  }

  "testPatternAsPlaceholderExpr" ignore AstFixture("") { cpg =>
    assertParse(
      "let 1️⃣<#name#> = 2️⃣<#value#>",
      substructure: VariableDeclSyntax(
        bindingSpecifier: .keyword(.let),
        bindings: [
          PatternBindingSyntax(
            pattern: IdentifierPatternSyntax(
              identifier: .identifier("<#name#>")
            ),
            initializer: InitializerClauseSyntax(
              value: DeclReferenceExprSyntax(
                baseName: .identifier("<#value#>")
              )
            )
          )
        ]
      ),
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )
  }
}
