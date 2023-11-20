

// This test file has been translated from swift/test/Parse/matching_patterns.swift

@_spi(ExperimentalLanguageFeatures) import SwiftParser
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class MatchingPatternsTests extends AbstractPassTest {
  "testMatchingPatterns1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      import imported_enums
      """
    )
  }

  "testMatchingPatterns3" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var x:Int
      """
    )
  }

  "testMatchingPatterns4" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func square(_ x: Int) -> Int { return x*x }
      """
    )
  }

  "testMatchingPatterns5" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct A<B> {
        struct C<D> { }
      }
      """
    )
  }

  "testMatchingPatterns6" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      switch x {
      // Expressions as patterns.
      case 0:
        ()
      case 1 + 2:
        ()
      case square(9):
        ()
      // 'var' and 'let' patterns.
      case var a:
        a = 1
      case let a:
        a = 1
      case inout a:
        a = 1
      case _mutating a:
        a = 1
      case _borrowing a:
        a = 1
      case _consuming a:
        a = 1
      case var var a:
        a += 1
      case var let a:
        print(a, terminator: "")
      case var (var b):
        b += 1
      // 'Any' pattern.
      case _:
        ()
      // patterns are resolved in expression-only positions are errors.
      case 1 + (_):
        ()
      }
      """#,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns7" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch (x,x) {
      case (var a, var a):
        fallthrough
      case _:
        ()
      }
      """
    )
  }

  "testMatchingPatterns8" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var e : Any = 0
      """
    )
  }

  "testMatchingPatterns7a" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch (x,x) {
      case _borrowing a:
        ()
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns7b" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch (x,x) {
      case _mutating a:
        ()
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns7c" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch (x,x) {
      case _consuming a:
        ()
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns9" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch e {
      // 'is' pattern.
      case is Int,
           is A<Int>,
           is A<Int>.C<Int>,
           is (Int, Int),
           is (a: Int, b: Int):
        ()
      }
      """
    )
  }

  "testMatchingPatterns10" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Enum patterns.
      enum Foo { case A, B, C }
      """
    )
  }

  "testMatchingPatterns11" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func == <T>(_: Voluntary<T>, _: Voluntary<T>) -> Bool { return true }
      """
    )
  }

  "testMatchingPatterns12" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Voluntary<T> : Equatable {
        case Naught
        case Mere(T)
        case Twain(T, T)
        func enumMethod(_ other: Voluntary<T>, foo: Foo) {
          switch self {
          case other:
            ()
          case .Naught,
               .Naught(),
               .Naught(_),
               .Naught(_, _):
            ()
          case .Mere,
               .Mere(),
               .Mere(_),
               .Mere(_, _):
            ()
          case .Twain(),
               .Twain(_),
               .Twain(_, _),
               .Twain(_, _, _):
            ()
          }
          switch foo {
          case .Naught:
            ()
          case .A, .B, .C:
            ()
          }
        }
      }
      """
    )
  }

  "testMatchingPatterns13" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var n : Voluntary<Int> = .Naught
      if case let .Naught(value) = n {}
      if case let .Naught(value1, value2, value3) = n {}
      if case inout .Naught(value) = n {}
      if case _mutating .Naught(value) = n {}
      if case _borrowing .Naught(value) = n {}
      if case _consuming .Naught(value) = n {}
      """
    )
  }

  "testMatchingPatterns14" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch n {
      case Foo.A:
        ()
      case Voluntary<Int>.Naught,
           Voluntary<Int>.Naught(),
           Voluntary<Int>.Naught(_, _),
           Voluntary.Naught,
           .Naught:
        ()
      case Voluntary<Int>.Mere,
           Voluntary<Int>.Mere(_),
           Voluntary<Int>.Mere(_, _),
           Voluntary.Mere,
           Voluntary.Mere(_),
           .Mere,
           .Mere(_):
        ()
      case .Twain,
           .Twain(_),
           .Twain(_, _),
           .Twain(_, _, _):
        ()
      }
      """
    )
  }

  "testMatchingPatterns15" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var notAnEnum = 0
      """
    )
  }

  "testMatchingPatterns16" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch notAnEnum {
      case .Foo:
        ()
      }
      """
    )
  }

  "testMatchingPatterns17" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct ContainsEnum {
        enum Possible<T> {
          case Naught
          case Mere(T)
          case Twain(T, T)
        }
        func member(_ n: Possible<Int>) {
          switch n {
          case ContainsEnum.Possible<Int>.Naught,
               ContainsEnum.Possible.Naught,
               Possible<Int>.Naught,
               Possible.Naught,
               .Naught:
            ()
          }
        }
      }
      """
    )
  }

  "testMatchingPatterns18" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func nonmemberAccessesMemberType(_ n: ContainsEnum.Possible<Int>) {
        switch n {
        case ContainsEnum.Possible<Int>.Naught,
             .Naught:
          ()
        }
      }
      """
    )
  }

  "testMatchingPatterns19" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var m : ImportedEnum = .Simple
      """
    )
  }

  "testMatchingPatterns20" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch m {
      case imported_enums.ImportedEnum.Simple,
           ImportedEnum.Simple,
           .Simple:
        ()
      case imported_enums.ImportedEnum.Compound,
           imported_enums.ImportedEnum.Compound(_),
           ImportedEnum.Compound,
           ImportedEnum.Compound(_),
           .Compound,
           .Compound(_):
        ()
      }
      """
    )
  }

  "testMatchingPatterns21" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Check that single-element tuple payloads work sensibly in patterns.
      """
    )
  }

  "testMatchingPatterns22" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum LabeledScalarPayload {
        case Payload(name: Int)
      }
      """
    )
  }

  "testMatchingPatterns23" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var lsp: LabeledScalarPayload = .Payload(name: 0)
      func acceptInt(_: Int) {}
      func acceptString(_: String) {}
      """
    )
  }

  "testMatchingPatterns24" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      switch lsp {
      case .Payload(0):
        ()
      case .Payload(name: 0):
        ()
      case let .Payload(x):
        acceptInt(x)
        acceptString("\(x)")
      case let .Payload(name: x):
        acceptInt(x)
        acceptString("\(x)")
      case let .Payload((name: x)):
        acceptInt(x)
        acceptString("\(x)")
      case .Payload(let (name: x)):
        acceptInt(x)
        acceptString("\(x)")
      case .Payload(let (name: x)):
        acceptInt(x)
        acceptString("\(x)")
      case .Payload(let x):
        acceptInt(x)
        acceptString("\(x)")
      case .Payload((let x)):
        acceptInt(x)
        acceptString("\(x)")
      case .Payload(inout x):
        acceptInt(x)
      case .Payload(_mutating x):
        acceptInt(x)
      case .Payload(_borrowing x):
        acceptInt(x)
      case .Payload(_consuming x):
        acceptInt(x)
      }
      """#,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns25" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Property patterns.
      """
    )
  }

  "testMatchingPatterns26" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S {
        static var stat: Int = 0
        var x, y : Int
        var comp : Int {
          return x + y
        }
        func nonProperty() {}
      }
      """
    )
  }

  "testMatchingPatterns27" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Tuple patterns.
      """
    )
  }

  "testMatchingPatterns28" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var t = (1, 2, 3)
      """
    )
  }

  "testMatchingPatterns29" ignore AstFixture("") { cpg =>
    assertParse(
      """
      prefix operator +++
      infix operator +++
      prefix func +++(x: (Int,Int,Int)) -> (Int,Int,Int) { return x }
      func +++(x: (Int,Int,Int), y: (Int,Int,Int)) -> (Int,Int,Int) {
        return (x.0+y.0, x.1+y.1, x.2+y.2)
      }
      """
    )
  }

  "testMatchingPatterns30" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch t {
      case (_, var a, 3):
        a += 1
      case (_, inout a, 3):
        a += 1
      case (_, _mutating a, 3):
        a += 1
      case (_, _borrowing a, 3):
        a += 1
      case (_, _consuming a, 3):
        a += 1
      case var (_, b, 3):
        b += 1
      case var (_, var c, 3):
        c += 1
      case (1, 2, 3):
        ()
      // patterns in expression-only positions are errors.
      case +++(_, var d, 3):
        ()
      case (_, var e, 3) +++ (1, 2, 3):
        ()
      case (let (_, _, _)) + 1:
        ()
      }
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testMatchingPatterns31" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // FIXME: We don't currently allow subpatterns for "isa" patterns that
      // require interesting conditional downcasts.
      class Base { }
      class Derived : Base { }
      """#
    )
  }

  "testMatchingPatterns32" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch [Derived(), Derived(), Base()] {
      case let ds as [Derived]:
        ()
      case is [Derived]:
        ()
      default:
        ()
      }
      """
    )
  }

  "testMatchingPatterns33" ignore AstFixture("") { cpg =>
    assertParse(
      """
      // Optional patterns.
      let op1 : Int?
      let op2 : Int??
      """
    )
  }

  "testMatchingPatterns34" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch op1 {
      case nil: break
      case 1?: break
      case _?: break
      }
      """
    )
  }

  "testMatchingPatterns35" ignore AstFixture("") { cpg =>
    assertParse(
      """
      switch op2 {
      case nil: break
      case _?: break
      case (1?)?: break
      case (_?)?: break
      }
      """
    )
  }

  "testMatchingPatterns36" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      // <rdar://problem/20365753> Bogus diagnostic "refutable pattern match can fail"
      let (responseObject: Int1️⃣?) = op1
      """#,
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '?' in tuple pattern")
      ]
    )
  }

  "testIfCaseMatchMutating" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case _mutating x = y {}
      guard case _mutating z = y else {}
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testIfCaseMatchConsuming" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case _consuming x = y {}
      guard case _consuming z = y else {}
      """,
      experimentalFeatures: .referenceBindings
    )
  }

  "testIfCaseMatchBorrowing" ignore AstFixture("") { cpg =>
    assertParse(
      """
      if case _borrowing x = y {}
      guard case _borrowing z = y else {}
      """,
      experimentalFeatures: .referenceBindings
    )
  }
}
