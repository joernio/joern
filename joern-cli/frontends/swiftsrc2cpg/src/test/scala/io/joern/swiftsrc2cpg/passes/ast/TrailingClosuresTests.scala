

// This test file has been translated from swift/test/Parse/trailing_closures.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TrailingClosuresTests extends AbstractPassTest {
  "testTrailingClosures1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo<T, U>(a: () -> T, b: () -> U) {}
      """
    )
  }

  "testTrailingClosures2" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      foo { 42 }
      b: { "" }
      """#
    )
  }

  "testTrailingClosures3" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      foo { 42 } b: { "" }
      """#
    )
  }

  "testTrailingClosures4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func when<T>(_ condition: @autoclosure () -> Bool,
                   `then` trueBranch: () -> T,
                   `else` falseBranch: () -> T) -> T {
        return condition() ? trueBranch() : falseBranch()
      }
      """
    )
  }

  "testTrailingClosures5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = when (2 < 3) { 3 } else: { 4 }
      """
    )
  }

  "testTrailingClosures6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S {
        static func foo(a: Int = 42, b: (inout Int) -> Void) -> S {
          return S()
        }
        static func foo(a: Int = 42, ab: () -> Void, b: (inout Int) -> Void) -> S {
          return S()
        }
        subscript(v v: () -> Int) -> Int {
          get { return v() }
        }
        subscript(u u: () -> Int, v v: () -> Int) -> Int {
          get { return u() + v() }
        }
        subscript(cond: Bool, v v: () -> Int) -> Int {
          get { return cond ? 0 : v() }
        }
        subscript(cond: Bool, u u: () -> Int, v v: () -> Int) -> Int {
          get { return cond ? u() : v() }
        }
      }
      """
    )
  }

  "testTrailingClosures7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: S = .foo {
        $0 = $0 + 1
      }
      """
    )
  }

  "testTrailingClosures8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: S = .foo {} b: { $0 = $0 + 1 }
      """
    )
  }

  "testTrailingClosures9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func bar(_ s: S) {
        _ = s[] {
          42
        }
        _ = s[] {
          21
        } v: {
          42
        }
        _ = s[true] {
          42
        }
        _ = s[true] {
          21
        } v: {
          42
        }
      }
      """
    )
  }

  "testTrailingClosures10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func multiple_trailing_with_defaults(
        duration: Int,
        animations: (() -> Void)? = nil,
        completion: (() -> Void)? = nil) {}
      """
    )
  }

  "testTrailingClosures11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      multiple_trailing_with_defaults(duration: 42) {}
      """
    )
  }

  "testTrailingClosures12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      multiple_trailing_with_defaults(duration: 42) {} completion: {}
      """
    )
  }

  "testTrailingClosures13a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      fn {} g: {}
      fn {} _: {}
      multiple {} _: { }
      mixed_args_1 {} _: {}
      mixed_args_1 {} a: {}  //  {{none}}
      mixed_args_2 {} a: {} _: {}
      mixed_args_2 {} _: {} //  {{none}}
      mixed_args_2 {} _: {} _: {} //  {{none}}
      """
    )
  }

  "testTrailingClosures13b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      fn {} g: 1️⃣<#T##() -> Void#>
      """,
      substructure: MultipleTrailingClosureElementSyntax(
        label: .identifier("g"),
        colon: .colonToken(trailingTrivia: .space),
        closure: ClosureExprSyntax(
          leftBrace: .leftBraceToken(presence: .missing),
          statements: CodeBlockItemListSyntax([
            CodeBlockItemSyntax(
              item: .init(DeclReferenceExprSyntax(baseName: .identifier("<#T##() -> Void#>")))
            )
          ]),
          rightBrace: .rightBraceToken(presence: .missing)
        )
      ),
      diagnostics: [
        DiagnosticSpec(message: "editor placeholder in source file")
      ]
    )
  }

  "testTrailingClosures14a" ignore AstFixture("") { cpg =>
    // TODO: The diagnostics here are perhaps a little overboard.
    assertParse(
      """
      func produce(fn: () -> Int?, default d: () -> Int) -> Int {
        return fn() ?? d()
      }
      _ = produce { 0 }1️⃣ 2️⃣default: { 1 }
      _ = produce { 2 } `default`: { 3 }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'default' label can only appear inside a 'switch' statement"),
      ],
      applyFixIts: ["insert newline"],
      fixedSource: """
        func produce(fn: () -> Int?, default d: () -> Int) -> Int {
          return fn() ?? d()
        }
        _ = produce { 0 }
        default: { 1 }
        _ = produce { 2 } `default`: { 3 }
        """
    )
  }

  "testTrailingClosures14b" ignore AstFixture("") { cpg =>
    // TODO: The diagnostics here are perhaps a little overboard.
    assertParse(
      """
      func produce(fn: () -> Int?, default d: () -> Int) -> Int {
        return fn() ?? d()
      }
      _ = produce { 0 }1️⃣ 2️⃣default: { 1 }
      _ = produce { 2 } `default`: { 3 }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'default' label can only appear inside a 'switch' statement"),
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: """
        func produce(fn: () -> Int?, default d: () -> Int) -> Int {
          return fn() ?? d()
        }
        _ = produce { 0 }; default: { 1 }
        _ = produce { 2 } `default`: { 3 }
        """
    )
  }

  "testTrailingClosures15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func f() -> Int { 42 }
      """
    )
  }

  "testTrailingClosures16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // This should be interpreted as a trailing closure, instead of being
      // interpreted as a computed property with undesired initial value.
      struct TrickyTest {
          var x : Int = f () {
              3
          }
      }
      """
    )
  }

}
