

// This test file has been translated from swift/test/Parse/subscripting.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SubscriptingTests extends AbstractPassTest {
  "testSubscripting1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X { }
      """
    )
  }

  "testSubscripting2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Simple examples
      struct X1 {
        var stored: Int
        subscript(i: Int) -> Int {
          get {
            return stored
          }
          mutating
          set {
            stored = newValue
          }
        }
      }
      """
    )
  }

  "testSubscripting3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X2 {
        var stored: Int
        subscript(i: Int) -> Int {
          get {
            return stored + i
          }
          set(v) {
            stored = v - i
          }
        }
      }
      """
    )
  }

  "testSubscripting4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X3 {
        var stored: Int
        subscript(_: Int) -> Int {
          get {
            return stored
          }
          set(v) {
            stored = v
          }
        }
      }
      """
    )
  }

  "testSubscripting5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X4 {
        var stored: Int
        subscript(i: Int, j: Int) -> Int {
          get {
            return stored + i + j
          }
          mutating
          set(v) {
            stored = v + i - j
          }
        }
      }
      """
    )
  }

  "testSubscripting6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X5 {
        static var stored: Int = 1
        static subscript(i: Int) -> Int {
          get {
            return stored + i
          }
          set {
            stored = newValue - i
          }
        }
      }
      """
    )
  }

  "testSubscripting7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class X6 {
        static var stored: Int = 1
        class subscript(i: Int) -> Int {
          get {
            return stored + i
          }
          set {
            stored = newValue - i
          }
        }
      }
      """
    )
  }

  "testSubscripting8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Y1 {
        var stored: Int
        subscript(_: i, j: Int) -> Int {
          get {
            return stored + j
          }
          set {
            stored = j
          }
        }
      }
      """
    )
  }

  "testSubscripting9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Mutating getters on constants
      // https://github.com/apple/swift/issues/43457
      """
    )
  }

  "testSubscripting10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Y2 {
        subscript(_: Int) -> Int {
          mutating get { return 0 }
        }
      }
      """
    )
  }

  "testSubscripting11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // FIXME: This test case does not belong in Parse/
      let y2 = Y2()
      _ = y2[0]
      """
    )
  }

  "testSubscripting12a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A0 {
        subscript 1️⃣
          i : 2️⃣Int3️⃣
           -> Int 4️⃣{
          get {
            return stored
          }
          set {
            stored = value
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '(' to start parameter clause", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected '(' to start function type", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected ')' in function type", fixIts: ["insert ')'"]),
        DiagnosticSpec(locationMarker: "4️⃣", message: "expected ')' to end parameter clause", fixIts: ["insert ')'"]),
        DiagnosticSpec(locationMarker: "4️⃣", message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"]),
      ],
      fixedSource: """
        struct A0 {
          subscript(
            i : (Int)
             -> Int) -> <#type#> {
            get {
              return stored
            }
            set {
              stored = value
            }
          }
        }
        """
    )
  }

  "testSubscripting12b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Parsing errors
      struct A0 {
        subscript 1️⃣-> Int {
          return 1
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in subscript", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        // Parsing errors
        struct A0 {
          subscript() -> Int {
            return 1
          }
        }
        """
    )
  }

  "testSubscripting13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A1 {
        subscript (i : Int) 1️⃣
           Int {
          get {
            return stored
          }
          set {
            stored = newValue
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '->' in subscript", fixIts: ["insert '->'"])
      ],
      fixedSource: """
        struct A1 {
          subscript (i : Int) ->
             Int {
            get {
              return stored
            }
            set {
              stored = newValue
            }
          }
        }
        """
    )
  }

  "testSubscripting14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A2 {
        subscript (i : Int) -> 1️⃣
           {
          get {
            return stored
          }
          set {
            stored = newValue
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected return type in subscript", fixIts: ["insert return type"])
      ],
      fixedSource: """
        struct A2 {
          subscript (i : Int) -> <#type#>
             {
            get {
              return stored
            }
            set {
              stored = newValue
            }
          }
        }
        """
    )
  }

  "testSubscripting15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A3 {
        subscript(i : Int) 1️⃣
        {
          get {
            return i
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"])
      ],
      fixedSource: """
        struct A3 {
          subscript(i : Int) -> <#type#>
          {
            get {
              return i
            }
          }
        }
        """
    )
  }

  "testSubscripting16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A4 {
        subscript(i : Int) 1️⃣{
          get {
            return i
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"])
      ],
      fixedSource: """
        struct A4 {
          subscript(i : Int) -> <#type#> {
            get {
              return i
            }
          }
        }
        """
    )
  }

  "testSubscripting17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A5 {
        subscript(i : Int) -> Int
      }
      """
    )
  }

  "testSubscripting18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A6 {
        subscript(i: Int)1️⃣(j: Int) -> Int {
          get {
            return i + j
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '(j: Int)' in subscript")
      ]
    )
  }

  "testSubscripting19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A7 {
        class subscript(a: Float) -> Int {
          get {
            return 42
          }
        }
      }
      """
    )
  }

  "testSubscripting20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class A7b {
        class static subscript(a: Float) -> Int {
          get {
            return 42
          }
        }
      }
      """
    )
  }

  "testSubscripting21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A8 ℹ️{
        subscript(i : Int) -> Int1️⃣
          get {
            return stored
          }
          set {
            stored = value
          }
        }2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected '{' in subscript",
          fixIts: ["insert '{'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected '}' to end struct",
          notes: [NoteSpec(message: "to match this opening '{'")],
          fixIts: ["insert '}'"]
        ),
      ],
      fixedSource: """
        struct A8 {
          subscript(i : Int) -> Int {
            get {
              return stored
            }
            set {
              stored = value
            }
          }
        }
        """
    )
  }

  "testSubscripting22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A9 {
        subscript 1️⃣x() -> Int {
          return 0
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "subscripts cannot have a name", fixIts: ["remove 'x'"])
      ],
      fixedSource: """
        struct A9 {
          subscript () -> Int {
            return 0
          }
        }
        """
    )
  }

  "testSubscripting23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A10 {
        subscript 1️⃣x(i: Int) -> Int {
          return 0
        }
        subscript 2️⃣x<T>(i: T) -> Int {
          return 0
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "subscripts cannot have a name", fixIts: ["remove 'x'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "subscripts cannot have a name", fixIts: ["remove 'x'"]),
      ],
      fixedSource: """
        struct A10 {
          subscript (i: Int) -> Int {
            return 0
          }
          subscript <T>(i: T) -> Int {
            return 0
          }
        }
        """
    )
  }

  "testSubscripting24" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A11 {
        subscript 1️⃣x y : 2️⃣Int 3️⃣-> Int 4️⃣{
          return 0
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '(' to start parameter clause", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected '(' to start function type", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected ')' in function type", fixIts: ["insert ')'"]),
        DiagnosticSpec(locationMarker: "4️⃣", message: "expected ')' to end parameter clause", fixIts: ["insert ')'"]),
        DiagnosticSpec(locationMarker: "4️⃣", message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"]),
      ],
      fixedSource: """
        struct A11 {
          subscript(x y : (Int) -> Int) -> <#type#> {
            return 0
          }
        }
        """
    )
  }
}
