

// This test file has been translated from swift/test/Parse/errors.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ErrorsTests extends AbstractPassTest {
  "testErrors1" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      enum MSV : Error {
        case Foo, Bar, Baz
        case CarriesInt(Int)
        var _domain: String { return "" }
        var _code: Int { return 0 }
      }
      """#
    )
  }

  "testErrors2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func opaque_error() -> Error { return MSV.Foo }
      """
    )
  }

  "testErrors3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        throw opaque_error()
      } catch MSV.Foo, MSV.CarriesInt(let num) {
      } catch {
      }
      """
    )
  }

  "testErrors4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func takesAutoclosure(_ fn : @autoclosure () -> Int) {}
      func takesThrowingAutoclosure(_ fn : @autoclosure () throws -> Int) {}
      """
    )
  }

  "testErrors5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func genError() throws -> Int { throw MSV.Foo }
      func genNoError() -> Int { return 0 }
      """
    )
  }

  "testErrors6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func testAutoclosures() throws {
        takesAutoclosure(genError())
        takesAutoclosure(genNoError())
        try takesAutoclosure(genError())
        try takesAutoclosure(genNoError())
        takesAutoclosure(try genError())
        takesAutoclosure(try genNoError())
        takesThrowingAutoclosure(try genError())
        takesThrowingAutoclosure(try genNoError())
        try takesThrowingAutoclosure(genError())
        try takesThrowingAutoclosure(genNoError())
        takesThrowingAutoclosure(genError())
        takesThrowingAutoclosure(genNoError())
      }
      """
    )
  }

  "testErrors7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func illformed() throws {
          do {
            _ = try genError()
          } catch MSV.CarriesInt(let i) where i == genError()1️⃣) {
          }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code ')' in 'catch' clause")
      ]
    )
  }

  "testErrors8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func postThrows() -> Int 1️⃣throws {
        return 5
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: """
        func postThrows() throws -> Int {
          return 5
        }
        """
    )
  }

  "testErrors9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func postThrows2() -> 1️⃣throws Int {
        return try postThrows()
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: """
        func postThrows2() throws -> Int {
          return try postThrows()
        }
        """
    )
  }

  "testErrors10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func postRethrows(_ f: () throws -> Int) -> Int 1️⃣rethrows {
        return try f()
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'rethrows' must precede '->'", fixIts: ["move 'rethrows' in front of '->'"])
      ],
      fixedSource: """
        func postRethrows(_ f: () throws -> Int) rethrows -> Int {
          return try f()
        }
        """
    )
  }

  "testErrors11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func postRethrows2(_ f: () throws -> Int) -> 1️⃣rethrows Int {
        return try f()
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'rethrows' must precede '->'", fixIts: ["move 'rethrows' in front of '->'"])
      ],
      fixedSource: """
        func postRethrows2(_ f: () throws -> Int) rethrows -> Int {
          return try f()
        }
        """
    )
  }

  "testErrors12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func postThrows3() {
        _ = { () -> Int 1️⃣throws in }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: """
        func postThrows3() {
          _ = { () throws -> Int in }
        }
        """
    )
  }

  "testErrors13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func dupThrows1() ℹ️throws 1️⃣rethrows -> 2️⃣throws Int 3️⃣throw {}
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "'rethrows' conflicts with 'throws'",
          notes: [NoteSpec(message: "'throws' declared here")],
          fixIts: ["remove redundant 'rethrows'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "'throws' must precede '->'",
          fixIts: ["remove redundant 'throws'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "'throw' must precede '->'",
          fixIts: ["remove redundant 'throw'"]
        ),
      ],
      fixedSource: """
        func dupThrows1() throws -> Int {}
        """
    )
  }

  "testErrors14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func dupThrows2(_ f: () throws -> 1️⃣rethrows Int) {}
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'rethrows' must precede '->'",
          fixIts: ["remove redundant 'rethrows'"]
        )
      ],
      fixedSource: """
        func dupThrows2(_ f: () throws -> Int) {}
        """
    )
  }

  "testErrors15a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = { () 1️⃣try ℹ️throws in }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'try' conflicts with 'throws'",
          notes: [NoteSpec(message: "'throws' declared here")],
          fixIts: ["remove redundant 'try'"]
        )
      ],
      fixedSource: """
        _ = { () throws in }
        """
    )
  }

  "testErrors15b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      _ = { () throws -> Int 1️⃣throws in }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'throws' must precede '->'",
          fixIts: ["remove redundant 'throws'"]
        )
      ],
      fixedSource: """
        _ = { () throws -> Int in }
        """
    )
  }

  "testErrors16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func incompleteThrowType() {
        let _: () 1️⃣throws
      }
      """,
      substructure: CodeBlockSyntax(
        statements: CodeBlockItemListSyntax([
          CodeBlockItemSyntax(
            item: .decl(
              DeclSyntax(
                VariableDeclSyntax(
                  bindingSpecifier: .keyword(.let),
                  bindings: PatternBindingListSyntax([
                    PatternBindingSyntax(
                      pattern: WildcardPatternSyntax(),
                      typeAnnotation: TypeAnnotationSyntax(type: TupleTypeSyntax(elements: TupleTypeElementListSyntax([])))
                    )
                  ])
                )
              )
            )
          )
        ]),
        UnexpectedNodesSyntax([TokenSyntax.keyword(.throws)])
      ),
      diagnostics: [
        DiagnosticSpec(message: "unexpected 'throws' keyword in function")
      ]
    )
  }

  "testErrors17a" ignore AstFixture("") { cpg =>
    // rdar://21328447
    assertParse(
      """
      func fixitThrow0() 1️⃣throw {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'throw' with 'throws'"])
      ],
      fixedSource: """
        func fixitThrow0() throws {}
        """
    )
  }

  "testErrors17b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitThrow1() 1️⃣throw -> Int {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'throw' with 'throws'"])
      ],
      fixedSource: """
        func fixitThrow1() throws -> Int {}
        """
    )
  }

  "testErrors17c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitThrow2() throws {
        var _: (Int)
        throw MSV.Foo
        var _: (Int) 1️⃣throw -> Int
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'throw' with 'throws'"])
      ],
      fixedSource: """
        func fixitThrow2() throws {
          var _: (Int)
          throw MSV.Foo
          var _: (Int) throws -> Int
        }
        """
    )
  }

  "testErrors18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let fn: () -> 1️⃣throws Void
      """,
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: """
        let fn: () throws -> Void
        """
    )
  }

  "testErrors19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // https://github.com/apple/swift/issues/53979
      """
    )
  }

  "testErrors20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitTry0<T>(a: T) 1️⃣try where T:ExpressibleByStringLiteral {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"])
      ],
      fixedSource: """
        func fixitTry0<T>(a: T) throws where T:ExpressibleByStringLiteral {}
        """
    )
  }

  "testErrors21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitTry1<T>(a: T) 1️⃣try {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"])
      ],
      fixedSource: """
        func fixitTry1<T>(a: T) throws {}
        """
    )
  }

  "testErrors22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitTry2() 1️⃣try {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"])
      ],
      fixedSource: """
        func fixitTry2() throws {}
        """
    )
  }

  "testErrors23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let fixitTry3 : () 1️⃣try -> Int
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"])
      ],
      fixedSource: """
        let fixitTry3 : () throws -> Int
        """
    )
  }

  "testErrors24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitAwait0() 1️⃣await { }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"])
      ],
      fixedSource: """
        func fixitAwait0() async { }
        """
    )
  }

  "testErrors25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitAwait1() 1️⃣await -> Int { }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"])
      ],
      fixedSource: """
        func fixitAwait1() async -> Int { }
        """
    )
  }

  "testErrors26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitAwait2() throws 1️⃣await -> Int { }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'await' must precede 'throws'",
          fixIts: ["move 'await' in front of 'throws'"]
        )
      ],
      fixedSource: """
        func fixitAwait2() async throws -> Int { }
        """
    )
  }

  "testErrors27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        true ? () : 1️⃣throw opaque_error()
      } catch _ {
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression after ternary operator", fixIts: ["insert expression"])
      ],
      fixedSource: """
        do {
          true ? () : <#expression#>throw opaque_error()
        } catch _ {
        }
        """
    )
  }

  "testErrors28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
      } catch {
        let error2 = error
      }
      """
    )
  }

  "testErrors29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
      } catch where true {
        let error2 = error
      } catch {
      }
      """
    )
  }

  // <rdar://problem/20985280> QoI: improve diagnostic on improper pattern match on type
  "testErrors30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        throw opaque_error()
      } catch MSV {
      } catch {
      }
      """
    )
  }

  "testErrors31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      do {
        throw opaque_error()
      } catch is Error {
      }
      """
    )
  }

  "testErrors32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() throws {}
        do {
      #if false
          try foo()
      #endif
        } catch {    // don't warn, #if code should be scanned.
        }
        do {
      #if false
          throw opaque_error()
      #endif
        } catch {    // don't warn, #if code should be scanned.
        }
      """
    )
  }

  "testAwaitBetwenAsyncAndThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitAwait2() ℹ️async 1️⃣await throws -> Int { }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'await' conflicts with 'async'",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'await'"]
        )
      ],
      fixedSource: """
        func fixitAwait2() async throws -> Int { }
        """
    )
  }

  "testAsyncAwait" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fixitAwait2() ℹ️async 1️⃣await -> Int { }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'await' conflicts with 'async'",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'await'"]
        )
      ],
      fixedSource: """
        func fixitAwait2() async -> Int { }
        """
    )
  }
}
