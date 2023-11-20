

// This test file has been translated from swift/test/Parse/self_rebinding.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SelfRebindingTests extends AbstractPassTest {
  "testSelfRebinding1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      class Writer {
          private var articleWritten = 47
          func stop() {
              let rest: () -> Void = { [weak self] in
                  let articleWritten = self?.articleWritten ?? 0
                  guard let `self` = self else {
                      return
                  }
                  self.articleWritten = articleWritten
              }
              fatalError("I'm running out of time")
              rest()
          }
          func nonStop() {
              let write: () -> Void = { [weak self] in
                  self?.articleWritten += 1
                  if let self = self {
                      self.articleWritten += 1
                  }
                  if let `self` = self {
                      self.articleWritten += 1
                  }
                  guard let self = self else {
                      return
                  }
                  self.articleWritten += 1
              }
              write()
          }
      }
      """#
    )
  }

  "testSelfRebinding2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct T {
          var mutable: Int = 0
          func f() {
              let 1️⃣self = self
          }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "keyword 'self' cannot be used as an identifier here", fixIts: ["if this name is unavoidable, use backticks to escape it"])
      ],
      fixedSource: """
        struct T {
            var mutable: Int = 0
            func f() {
                let `self` = self
            }
        }
        """
    )
  }

  "testSelfRebinding3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class MyCls {
          func something() {}
          func test() {
              let `self` = Writer() // Even if `self` is shadowed,
              something() // this should still refer `MyCls.something`.
          }
      }
      """
    )
  }

  "testSelfRebinding4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/47136
      // Method called 'self' can be confused with regular 'self'
      """
    )
  }

  "testSelfRebinding5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func funcThatReturnsSomething(_ any: Any) -> Any {
          any
      }
      """
    )
  }

  "testSelfRebinding6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct TypeWithSelfMethod {
          let property = self
          // Existing warning expected, not confusable
          let property2 = self()
          let propertyFromClosure: () = {
              print(self)
          }()
          let propertyFromFunc = funcThatReturnsSomething(self)
          let propertyFromFunc2 = funcThatReturnsSomething(TypeWithSelfMethod.self) // OK
          func `self`() {
          }
      }
      """
    )
  }

  "testSelfRebinding7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /// Test fix_unqualified_access_member_named_self doesn't appear for computed var called `self`
      /// it can't currently be referenced as a static member -- unlike a method with the same name
      struct TypeWithSelfComputedVar {
          let property = self
          let propertyFromClosure: () = {
              print(self)
          }()
          let propertyFromFunc = funcThatReturnsSomething(self)
          var `self`: () {
              ()
          }
      }
      """
    )
  }

  "testSelfRebinding8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      /// Test fix_unqualified_access_member_named_self doesn't appear for property called `self`
      /// it can't currently be referenced as a static member -- unlike a method with the same name
      struct TypeWithSelfProperty {
          let property = self
          let propertyFromClosure: () = {
              print(self)
          }()
          let propertyFromFunc = funcThatReturnsSomething(self)
          let `self`: () = ()
      }
      """
    )
  }

  "testSelfRebinding9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum EnumCaseNamedSelf {
          case `self`
          init() {
              self = .self // OK
              self = .`self` // OK
              self = EnumCaseNamedSelf.`self` // OK
          }
      }
      """
    )
  }

  "testSelfRebinding10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // rdar://90624344 - warning about `self` which cannot be fixed because it's located in implicitly generated code.
      struct TestImplicitSelfUse : Codable {
        let `self`: Int // Ok
      }
      """
    )
  }

}
