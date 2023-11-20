

// This test file has been translated from swift/test/Parse/optional.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class OptionalTests extends AbstractPassTest {
  "testOptional1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A {
        func foo() {}
      }
      """
    )
  }

  "testOptional2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var a : A?
      var b : A 1️⃣?
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous code '?' at top level")
      ]
    )
  }

  "testOptional3a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var c = a?
      """,
      substructure: OptionalChainingExprSyntax(
        expression: DeclReferenceExprSyntax(baseName: .identifier("a")),
        questionMark: .postfixQuestionMarkToken()
      )
    )
  }

  "testOptional3b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var d : ()? = a?.foo()
      """
    )
  }

  "testOptional4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var e : (() -> A)?
      var f = e?()
      """
    )
  }

  "testOptional5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct B<T> {}
      var g = B<A?>()
      """
    )
  }

}
