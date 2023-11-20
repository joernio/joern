

// This test file has been translated from swift/test/Parse/try.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class TryTests extends AbstractPassTest {
  "testTry1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Intentionally has lower precedence than assignments and ?:
      infix operator %%%% : LowPrecedence
      precedencegroup LowPrecedence {
        associativity: none
        lowerThan: AssignmentPrecedence
      }
      func %%%%<T, U>(x: T, y: U) -> Int { return 0 }
      """
    )
  }

  "testTry2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Intentionally has lower precedence between assignments and ?:
      infix operator %%% : MiddlingPrecedence
      precedencegroup MiddlingPrecedence {
        associativity: none
        higherThan: AssignmentPrecedence
        lowerThan: TernaryPrecedence
      }
      func %%%<T, U>(x: T, y: U) -> Int { return 1 }
      """
    )
  }

  "testTry3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() throws -> Int { return 0 }
      func bar() throws -> Int { return 0 }
      """
    )
  }

  "testTry4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var x = try foo() + bar()
      x = try foo() + bar()
      x += try foo() + bar()
      x += try foo() %%%% bar()
      x += try foo() %%% bar()
      x = foo() + try bar()
      """
    )
  }

  "testTry5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var y = true ? try foo() : try bar() + 0
      var z = true ? try foo() : try bar() %%% 0
      """
    )
  }

  "testTry6" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var a = try! foo() + bar()
      a = try! foo() + bar()
      a += try! foo() + bar()
      a += try! foo() %%%% bar()
      a += try! foo() %%% bar()
      a = foo() + try! bar()
      """
    )
  }

  "testTry7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var b = true ? try! foo() : try! bar() + 0
      var c = true ? try! foo() : try! bar() %%% 0
      """
    )
  }

  "testTry8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      infix operator ?+= : AssignmentPrecedence
      func ?+=(lhs: inout Int?, rhs: Int?) {
        lhs = lhs! + rhs!
      }
      """
    )
  }

  "testTry9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var i = try? foo() + bar()
      let _: Double = i
      i = try? foo() + bar()
      i ?+= try? foo() + bar()
      i ?+= try? foo() %%%% bar()
      i ?+= try? foo() %%% bar()
      _ = foo() == try? bar()
      _ = (try? foo()) == bar()
      _ = foo() == (try? bar())
      _ = (try? foo()) == (try? bar())
      """
    )
  }

  "testTry10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let j = true ? try? foo() : try? bar() + 0
      let k = true ? try? foo() : try? bar() %%% 0
      """
    )
  }

  "testTry11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try let singleLet = try foo()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["remove redundant 'try'"])
      ],
      fixedSource: "let singleLet = try foo()"
    )
  }

  "testTry11a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try let singleLet = foo()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: "let singleLet = try foo()"
    )
  }

  "testTry11b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try var singleVar = foo()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: "var singleVar = try foo()"
    )
  }

  "testTry11c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try let uninit: Int
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: [])
      ]
    )
  }

  "testTry11d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try let (destructure1, destructure2) = (foo(), bar())
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: "let (destructure1, destructure2) = try (foo(), bar())"
    )
  }

  "testTry11e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try let multi1 = foo(), multi2 = bar()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: "let multi1 = try foo(), multi2 = try bar()"
    )
  }

  "testTry11f" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class TryDecl {
        1️⃣try let singleLet = foo()
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: """
        class TryDecl {
          let singleLet = try foo()
        }
        """
    )
  }

  "testTry11g" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class TryDecl {
        1️⃣try var singleVar = foo()
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the initial value expression", fixIts: ["move 'try' after '='"])
      ],
      fixedSource: """
        class TryDecl {
          var singleVar = try foo()
        }
        """
    )
  }

  "testTry11h" ignore AstFixture("") { cpg =>
    assertParse(
      """
      try1️⃣
      func method() {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression after 'try'", fixIts: ["insert expression"])
      ],
      fixedSource: """
        try <#expression#>
        func method() {}
        """
    )
  }

  "testTry11i" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try func method() {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' cannot be used with 'func'")
      ]
    )
  }

  "testTry12a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try while true {
        2️⃣try break
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'try' cannot be used with 'while'"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'try' cannot be used with 'break'"),
      ]
    )
  }

  "testTry12b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try throw 2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'try' must be placed on the thrown expression", fixIts: ["move 'try' after 'throw'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected expression after 'try'", fixIts: ["insert expression"]),
      ],
      fixedSource: "throw try <#expression#>"
    )
  }

  "testTry12c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try return
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' cannot be used with 'return'")
      ]
    )
  }

  "testTry12d" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try throw foo()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the thrown expression", fixIts: ["move 'try' after 'throw'"])
      ],
      fixedSource: "throw try foo()"
    )
  }

  "testTry12e" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣try return foo()
      """,
      diagnostics: [
        DiagnosticSpec(message: "'try' must be placed on the returned expression", fixIts: ["move 'try' after 'return'"])
      ],
      fixedSource: "return try foo()"
    )
  }

  "testTry13" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // Test operators.
      func *(a : String, b : String) throws -> Int { return 42 }
      let _ = "foo"
              *
              "bar"
      let _ = try! "foo"*"bar"
      let _ = try? "foo"*"bar"
      let _ = (try? "foo"*"bar") ?? 0
      """#
    )
  }

  "testTry14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // <rdar://problem/21414023> Assertion failure when compiling function that takes throwing functions and rethrows
      func rethrowsDispatchError(handleError: ((Error) throws -> ()), body: () throws -> ()) rethrows {
        do {
          body()
        } catch {
        }
      }
      """
    )
  }

  "testTry15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // <rdar://problem/21432429> Calling rethrows from rethrows crashes Swift compiler
      struct r21432429 {
        func x(_ f: () throws -> ()) rethrows {}
        func y(_ f: () throws -> ()) rethrows {
          x(f)
        }
      }
      """
    )
  }

  "testTry16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // <rdar://problem/21427855> Swift 2: Omitting try from call to throwing closure in rethrowing function crashes compiler
      func callThrowingClosureWithoutTry(closure: (Int) throws -> Int) rethrows {
        closure(0)
      }
      """
    )
  }

  "testTry17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func producesOptional() throws -> Int? { return nil }
      let _: String = try? producesOptional()
      """
    )
  }

  "testTry18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _ = (try? foo())!!
      """
    )
  }

  "testTry19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func producesDoubleOptional() throws -> Int?? { return 3 }
      let _: String = try? producesDoubleOptional()
      """
    )
  }

  "testTry20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func maybeThrow() throws {}
      try maybeThrow() // okay
      try! maybeThrow() // okay
      try? maybeThrow() // okay since return type of maybeThrow is Void
      _ = try? maybeThrow() // okay
      """
    )
  }

  "testTry21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: () -> Void = { try! maybeThrow() } // okay
      let _: () -> Void = { try? maybeThrow() } // okay since return type of maybeThrow is Void
      """
    )
  }

  "testTry22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if try? maybeThrow() {
      }
      let _: Int = try? foo()
      """
    )
  }

  "testTry23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class X {}
      func test(_: X) {}
      func producesObject() throws -> AnyObject { return X() }
      test(try producesObject())
      """
    )
  }

  "testTry24" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = "a\(try maybeThrow())b"
      _ = try "a\(maybeThrow())b"
      _ = "a\(maybeThrow())"
      """#
    )
  }

  "testTry25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension DefaultStringInterpolation {
        mutating func appendInterpolation() throws {}
      }
      """
    )
  }

  "testTry26" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      _ = try "a\()b"
      _ = "a\()b"
      _ = try "\() \(1)"
      """#
    )
  }

  "testTry27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func testGenericOptionalTry<T>(_ call: () throws -> T ) {
        let _: String = try? call()
      }
      """
    )
  }

  "testTry28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func genericOptionalTry<T>(_ call: () throws -> T ) -> T? {
        let x = try? call() // no error expected
        return x
      }
      """
    )
  }

  "testTry29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Test with a non-optional type
      let _: String = genericOptionalTry({ () throws -> Int in return 3 })
      """
    )
  }

  "testTry30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Test with an optional type
      let _: String = genericOptionalTry({ () throws -> Int? in return nil })
      """
    )
  }

  "testTry31" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func produceAny() throws -> Any {
        return 3
      }
      """
    )
  }

  "testTry32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: Int? = try? produceAny() as? Int
      let _: Int?? = (try? produceAny()) as? Int // good
      let _: String = try? produceAny() as? Int
      let _: String = (try? produceAny()) as? Int
      """
    )
  }

  "testTry33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct ThingProducer {
        func produceInt() throws -> Int { return 3 }
        func produceIntNoThrowing() -> Int { return 3 }
        func produceAny() throws -> Any { return 3 }
        func produceOptionalAny() throws -> Any? { return 3 }
        func produceDoubleOptionalInt() throws -> Int?? { return 3 }
      }
      """
    )
  }

  "testTry34" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let optProducer: ThingProducer? = ThingProducer()
      let _: Int? = try? optProducer?.produceInt()
      let _: Int = try? optProducer?.produceInt()
      let _: String = try? optProducer?.produceInt()
      let _: Int?? = try? optProducer?.produceInt() // good
      """
    )
  }

  "testTry35" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: Int? = try? optProducer?.produceIntNoThrowing()
      let _: Int?? = try? optProducer?.produceIntNoThrowing()
      """
    )
  }

  "testTry36" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: Int? = (try? optProducer?.produceAny()) as? Int // good
      let _: Int? = try? optProducer?.produceAny() as? Int
      let _: Int?? = try? optProducer?.produceAny() as? Int // good
      let _: String = try? optProducer?.produceAny() as? Int
      """
    )
  }

  "testTry37" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: String = try? optProducer?.produceDoubleOptionalInt()
      """
    )
  }

  "testTry38" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let producer = ThingProducer()
      """
    )
  }

  "testTry39" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let _: Int = try? producer.produceDoubleOptionalInt()
      let _: Int? = try? producer.produceDoubleOptionalInt()
      let _: Int?? = try? producer.produceDoubleOptionalInt()
      let _: Int??? = try? producer.produceDoubleOptionalInt() // good
      let _: String = try? producer.produceDoubleOptionalInt()
      """
    )
  }

  "testTry40" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // rdar://problem/46742002
      protocol Dummy : class {}
      """
    )
  }

  "testTry41" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class F<T> {
        func wait() throws -> T { fatalError() }
      }
      """
    )
  }

  "testTry42" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func bar(_ a: F<Dummy>, _ b: F<Dummy>) {
        _ = (try? a.wait()) === (try? b.wait())
      }
      """
    )
  }
}
