

// This test file has been translated from swift/test/Parse/pattern_without_variables.swift

@_spi(ExperimentalLanguageFeatures) import SwiftParser
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PatternWithoutVariablesTests extends AbstractPassTest {
  "testPatternWithoutVariables1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = 1
      inout _ = 1
      _mutating _ = 1
      _borrowing _ = 1
      _consuming _ = 1
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testPatternWithoutVariables2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() {
        let _ = 1 // OK
        inout _ = 1
        _mutating _ = 1
        _borrowing _ = 1
        _consuming _ = 1
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testPatternWithoutVariables3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        let _ = 1
        var (_, _) = (1, 2)
        func foo() {
          let _ = 1 // OK
        }
        inout (_, _) = (1, 2)
        _mutating (_, _) = (1, 2)
        _borrowing (_, _) = (1, 2)
        _consuming (_, _) = (1, 2)
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testPatternWithoutVariables4" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // <rdar://problem/19786845> Warn on "let" and "var" when no data is bound in a pattern
      enum SimpleEnum { case Bar }
      """#
    )
  }

  "testPatternWithoutVariables5" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      func testVarLetPattern(a : SimpleEnum) {
        switch a {
        case let .Bar: break
        }
        switch a {
        case let x: _ = x; break         // Ok.
        }
        switch a {
        case let _: break
        }
        switch (a, 42) {
        case let (_, x): _ = x; break    // ok
        }
        if case let _ = "str" {}
        switch a {
        case inout .Bar: break
        }
        switch a {
        case _mutating .Bar: break
        }
        switch a {
        case _borrowing .Bar: break
        }
        switch a {
        case _consuming .Bar: break
        }
      }
      """#
    )
  }

  "testPatternWithoutVariables6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/53293
      class C_53293 {
        static var _: Int { 0 }
      }
      """
    )
  }

  "testMutatingNotADeclarationStartIfNotEnabled" ignore AstFixture("") { cpg =>
    assertParse("_mutating = 2")
  }
}
