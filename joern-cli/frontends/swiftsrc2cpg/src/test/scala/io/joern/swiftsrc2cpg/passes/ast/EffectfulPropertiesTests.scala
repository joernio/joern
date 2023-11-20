

// This test file has been translated from swift/test/Parse/effectful_properties.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class EffectfulPropertiesTests extends AbstractPassTest {
  "testEffectfulProperties1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct MyProps {
        var prop1 : Int {
          get async { }
        }
        var prop2 : Int {
          get throws { }
        }
        var prop3 : Int {
          get async throws { }
        }
        var prop1mut : Int {
          mutating get async { }
        }
        var prop2mut : Int {
          mutating get throws { }
        }
        var prop3mut : Int {
          mutating get async throws { }
        }
      }
      """
    )
  }

  "testEffectfulProperties2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X1 {
        subscript(_ i : Int) -> Int {
            get async {}
          }
      }
      """
    )
  }

  "testEffectfulProperties3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class X2 {
        subscript(_ i : Int) -> Int {
            get throws {}
          }
      }
      """
    )
  }

  "testEffectfulProperties4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X3 {
        subscript(_ i : Int) -> Int {
            get async throws {}
          }
      }
      """
    )
  }

  "testEffectfulProperties5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct BadSubscript1 {
        subscript(_ i : Int) -> Int {
            get async throws {}
            set {}
          }
      }
      """
    )
  }

  "testEffectfulProperties6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct BadSubscript2 {
        subscript(_ i : Int) -> Int {
            get throws {}
            set throws {}
          }
      }
      """
    )
  }

  "testEffectfulProperties7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S {
        var prop2 : Int {
          mutating get async throws { 0 }
          nonmutating set {}
        }
      }
      """
    )
  }

  "testEffectfulProperties8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var prop3 : Bool {
        _read { yield prop3 }
        get throws { false }
        get async { true }
        get {}
      }
      """
    )
  }

  "testEffectfulProperties9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum E {
        private(set) var prop4 : Double {
          set {}
          get async throws { 1.1 }
          _modify { yield &prop4 }
        }
      }
      """
    )
  }

  "testEffectfulProperties10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol P {
        associatedtype T
        var prop1 : T { get async throws }
        var prop2 : T { get async throws set }
        var prop3 : T { get throws set }
        var prop4 : T { get async }
        var prop5 : T { mutating get async throws }
        var prop6 : T { mutating get throws }
        var prop7 : T { mutating get async nonmutating set }
      }
      """
    )
  }

  "testEffectfulProperties11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      ///////////////////
      // invalid syntax
      """
    )
  }

  "testEffectfulProperties12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad1 : Int {
        get 1️⃣rethrows { 0 }
        set 2️⃣rethrows { }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected throwing specifier; did you mean 'throws'?",
          fixIts: ["replace 'rethrows' with 'throws'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected throwing specifier; did you mean 'throws'?",
          fixIts: ["replace 'rethrows' with 'throws'"]
        ),
      ],
      fixedSource: """
        var bad1 : Int {
          get throws { 0 }
          set throws { }
        }
        """
    )
  }

  "testEffectfulProperties13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad2 : Int {
        get 1️⃣reasync { 0 }
        set 2️⃣reasync { }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected async specifier; did you mean 'async'?",
          fixIts: ["replace 'reasync' with 'async'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected async specifier; did you mean 'async'?",
          fixIts: ["replace 'reasync' with 'async'"]
        ),
      ],
      fixedSource: """
        var bad2 : Int {
          get async { 0 }
          set async { }
        }
        """
    )
  }

  "testEffectfulProperties14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad3 : Int {
        _read async { yield 0 }
        set(theValue) async { }
      }
      """
    )
  }

  "testEffectfulProperties15a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad4 : Int = 0 {
        willSet(theValue) 1️⃣reasync 2️⃣rethrows async 3️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected async specifier; did you mean 'async'?",
          fixIts: ["replace 'reasync' with 'async'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "'rethrows' conflicts with 'throws'",
          notes: [NoteSpec(locationMarker: "3️⃣", message: "'throws' declared here")],
          fixIts: ["remove redundant 'rethrows'"]
        ),
      ],
      fixedSource: """
        var bad4 : Int = 0 {
          willSet(theValue) async async throws {}
        }
        """
    )
  }

  "testEffectfulProperties15b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad4 : Int = 0 {
        didSet throws 1️⃣bogus {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'bogus' in accessor")
      ]
    )
  }

  "testEffectfulProperties16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad5 : Int {
        get 1️⃣bogus 2️⃣rethrows {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code 'bogus rethrows' in accessor")
      ]
    )
  }

  "testEffectfulProperties17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad6 : Int {
        get 1️⃣rethrows 2️⃣-> Int { 0 }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected throwing specifier; did you mean 'throws'?",
          fixIts: ["replace 'rethrows' with 'throws'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '-> Int' in accessor"),
      ],
      fixedSource: """
        var bad6 : Int {
          get throws -> Int { 0 }
        }
        """
    )
  }

  "testEffectfulProperties18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad7 : Double {
        get throws 1️⃣async { 3.14 }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"])
      ],
      fixedSource: """
        var bad7 : Double {
          get async throws { 3.14 }
        }
        """
    )
  }

  "testEffectfulProperties19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad8 : Double {
        get {}
        _modify throws 1️⃣async { yield &bad8 }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"])
      ],
      fixedSource: """
        var bad8 : Double {
          get {}
          _modify async throws { yield &bad8 }
        }
        """
    )
  }

  "testEffectfulProperties20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol BadP {
        var prop2 : Int { get 1️⃣bogus rethrows set }
        var prop3 : Int { get 2️⃣rethrows 3️⃣bogus set }
        var prop4 : Int { get 4️⃣reasync 5️⃣bogus set }
        var prop5 : Int { get throws 6️⃣async }
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code 'bogus rethrows set' in variable"),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected throwing specifier; did you mean 'throws'?",
          fixIts: ["replace 'rethrows' with 'throws'"]
        ),
        DiagnosticSpec(locationMarker: "3️⃣", message: "unexpected code 'bogus set' in variable"),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "expected async specifier; did you mean 'async'?",
          fixIts: ["replace 'reasync' with 'async'"]
        ),
        DiagnosticSpec(locationMarker: "5️⃣", message: "unexpected code 'bogus set' in variable"),
        DiagnosticSpec(
          locationMarker: "6️⃣",
          message: "'async' must precede 'throws'",
          fixIts: ["move 'async' in front of 'throws'"]
        ),
      ],
      fixedSource: """
        protocol BadP {
          var prop2 : Int { get bogus rethrows set }
          var prop3 : Int { get throws bogus set }
          var prop4 : Int { get async bogus set }
          var prop5 : Int { get async throws }
        }
        """
    )
  }
}
