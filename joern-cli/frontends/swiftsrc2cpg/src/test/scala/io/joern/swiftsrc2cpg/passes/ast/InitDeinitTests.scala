

// This test file has been translated from swift/test/Parse/init_deinit.swift

import SwiftSyntax
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class InitDeinitTests extends AbstractPassTest {
  "testInitDeinit1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorA {
        init 1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        struct FooStructConstructorA {
          init()
        }
        """
    )
  }

  "testInitDeinit2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorB {
        init()
      }
      """
    )
  }

  "testInitDeinit3a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorC {
        init 1️⃣{}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        struct FooStructConstructorC {
          init() {}
        }
        """
    )
  }

  "testInitDeinit3b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorC {
        init<T> 1️⃣{}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        struct FooStructConstructorC {
          init<T>() {}
        }
        """
    )
  }

  "testInitDeinit3c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorC {
        init? 1️⃣{ self.init() }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        struct FooStructConstructorC {
          init?() { self.init() }
        }
        """
    )
  }

  "testInitDeinit4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructConstructorD {
        init() 1️⃣-> FooStructConstructorD { }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "initializers cannot have a result type")
      ]
    )
  }

  "testInitDeinit5a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructDeinitializerA {
        deinit
      }
      """
    )
  }

  "testInitDeinit5b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructDeinitializerA {
        deinit 1️⃣x
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have a name", fixIts: ["remove 'x'"])
      ],
      fixedSource: """
        struct FooStructDeinitializerA {
          deinit
        }
        """
    )
  }

  "testInitDeinit5c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructDeinitializerA {
        deinit 1️⃣x()
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "deinitializers cannot have a name and parameters",
          highlight: "x()",
          fixIts: ["remove 'x' and parameter clause"]
        )
      ],
      fixedSource: """
        struct FooStructDeinitializerA {
          deinit
        }
        """
    )
  }

  "testInitDeinit6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructDeinitializerB {
        deinit
      }
      """
    )
  }

  "testInitDeinit7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct FooStructDeinitializerC {
        deinit {}
      }
      """
    )
  }

  "testInitDeinit8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit1️⃣(a : Int) {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have parameters", fixIts: ["remove parameter clause"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testInitDeinit9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerB {
        deinit { }
      }
      """
    )
  }

  "testInitDeinit10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerC {
        deinit 1️⃣x (a : Int) {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "deinitializers cannot have a name and parameters",
          highlight: "x (a : Int)",
          fixIts: ["remove 'x' and parameter clause"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerC {
          deinit {}
        }
        """
    )
  }

  "testInitDeinit11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      init 1️⃣{}
      init()
      init() {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        init() {}
        init()
        init() {}
        """
    )
  }

  "testInitDeinit12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      deinit {}
      deinit
      deinit {}
      """
    )
  }

  "testInitDeinit13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct BarStruct {
        init() {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension BarStruct {
        init(x : Int) {}
        // When/if we allow 'var' in extensions, then we should also allow dtors
        deinit {}
      }
      """
    )
  }

  "testInitDeinit15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum BarUnion {
        init() {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension BarUnion {
        init(x : Int) {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class BarClass {
        init() {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension BarClass {
        convenience init(x : Int) { self.init() }
        deinit {}
      }
      """
    )
  }

  "testInitDeinit19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol BarProtocol {
        init() {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension BarProtocol {
        init(x : Int) {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fooFunc() {
        init() {}
        deinit {}
      }
      """
    )
  }

  "testInitDeinit22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func barFunc() {
        var x : () = { () -> () in
          init() {}
          return
        } ()
        var y : () = { () -> () in
          deinit {}
          return
        } ()
      }
      """
    )
  }

  "testInitDeinit24" ignore AstFixture("") { cpg =>
    // https://github.com/apple/swift/issues/43464
    assertParse(
      """
      class Aaron {
        convenience init() { init(x: 1) }
      }
      """
    )
  }

  "testInitDeinit25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Theodosia: Aaron {
        init() {
          init(x: 2)
        }
      }
      """
    )
  }

  "testInitDeinit26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct AaronStruct {
        init(x: Int) {}
        init() { init(x: 1) }
      }
      """
    )
  }

  "testInitDeinit27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum AaronEnum: Int {
        case A = 1
        init(x: Int) { init(rawValue: x)! }
      }
      """
    )
  }

  "testInitDeinit28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      init(_ foo: T) 1️⃣-> Int where T: Comparable {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "initializers cannot have a result type")
      ]
    )
  }

  "testDeinitInSwiftinterfaceIsFollowedByFinalFunc" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Foo {
        deinit
        final func foo()
      }
      """,
      substructure: DeinitializerDeclSyntax()
    )
  }

  "testDeinitAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit async {}
      }
      """
    )
  }

  "testDeinitAwait" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣await {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitReasync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣reasync {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'reasync' with 'async'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot throw", fixIts: ["remove 'throws'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitRethrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣rethrows {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot throw", fixIts: ["remove 'rethrows'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitAsyncAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit ℹ️async 1️⃣async {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'async' has already been specified",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'async'"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAsyncAwait" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit ℹ️async 1️⃣await {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'await' conflicts with 'async'",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'await'"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAwaitAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣await ℹ️async {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'await' conflicts with 'async'",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'await'"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAsyncThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit async 1️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot throw", fixIts: ["remove 'throws'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitThrowsAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣throws async {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot throw", fixIts: ["remove 'throws'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitThrowsAsyncRethrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣throws async rethrows {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot throw", fixIts: ["remove 'throws' and 'rethrows'"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitReasyncThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣reasync 2️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'reasync' with 'async'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "deinitializers cannot throw", fixIts: ["remove 'throws'"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitNameAwait" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x 2️⃣await {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "deinitializers cannot have a name", fixIts: ["remove 'x'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitNameAsyncAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x ℹ️async 2️⃣async {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "deinitializers cannot have a name", fixIts: ["remove 'x'"]),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "'async' has already been specified",
          notes: [NoteSpec(message: "'async' declared here")],
          fixIts: ["remove redundant 'async'"]
        ),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitNameAsyncThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x async 2️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "deinitializers cannot have a name", fixIts: ["remove 'x'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "deinitializers cannot throw", fixIts: ["remove 'throws'"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitParamsAsyncThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit1️⃣() async 2️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "deinitializers cannot have parameters", fixIts: ["remove parameter clause"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "deinitializers cannot throw", fixIts: ["remove 'throws'"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitNameParamsThrowsAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x() 2️⃣throws async {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "deinitializers cannot have a name and parameters",
          highlight: "x()",
          fixIts: ["remove 'x' and parameter clause"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "deinitializers cannot throw",
          fixIts: ["remove 'throws'"]
        ),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitNameParamsAwaitThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x() 2️⃣await 3️⃣throws {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "deinitializers cannot have a name and parameters",
          highlight: "x()",
          fixIts: ["remove 'x' and parameter clause"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected async specifier; did you mean 'async'?",
          fixIts: ["replace 'await' with 'async'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "deinitializers cannot throw",
          fixIts: ["remove 'throws'"]
        ),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitOutput" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣-> Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->' and return type"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitNameOutput" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x -> Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "deinitializers cannot have a name and return clause",
          highlight: "x -> Void",
          fixIts: ["remove 'x', '->', and return type"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitParamsOutput" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit1️⃣() -> Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "deinitializers cannot have parameters and return clause",
          highlight: "() -> Void",
          fixIts: ["remove parameter clause, '->', and return type"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitNameParamsOutput" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣x() -> Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "deinitializers cannot have a name, parameters, and return clause",
          highlight: "x() -> Void",
          fixIts: ["remove 'x', parameter clause, '->', and return type"]
        )
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitOutputAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣-> async Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'async' in effect specifiers", fixIts: ["insert 'async'"]),
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'async', and return type"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAsyncOutputAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit async 1️⃣-> async Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'async', and return type"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAwaitOutputAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣await 2️⃣-> async Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'async', and return type"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitAsyncOutputAwait" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit async 1️⃣-> await Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'await', and return type"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitOutputThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣-> throws Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'throws', and return type"])
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitThrowsOutputThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣throws 2️⃣-> throws Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "deinitializers cannot throw", fixIts: ["remove 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'throws', and return type"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit {}
        }
        """
    )
  }

  "testDeinitOutputAsyncThrows" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣-> async throws Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'async' in effect specifiers", fixIts: ["insert 'async'"]),
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'async throws', and return type"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testDeinitOutputThrowsAsync" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class FooClassDeinitializerA {
        deinit 1️⃣-> throws async Void {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'async' in effect specifiers", fixIts: ["insert 'async'"]),
        DiagnosticSpec(message: "deinitializers cannot have a return clause", fixIts: ["remove '->', 'throws async', and return type"]),
      ],
      fixedSource: """
        class FooClassDeinitializerA {
          deinit async {}
        }
        """
    )
  }

  "testAsyncDeinit" ignore AstFixture("") { cpg =>
    // This is expected for now.
    // `async` is parsed as a modifier like `public` because you can have an `async var x: Int`.
    assertParse(
      """
      class FooClassDeinitializerA {
        async deinit {}
      }
      """
    )
  }
}
