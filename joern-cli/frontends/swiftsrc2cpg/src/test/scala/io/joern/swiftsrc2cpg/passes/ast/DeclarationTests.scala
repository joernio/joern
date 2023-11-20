

import SwiftBasicFormat
@_spi(Testing) @_spi(RawSyntax) @_spi(ExperimentalLanguageFeatures) import SwiftParser
@_spi(RawSyntax) import SwiftSyntax
import SwiftSyntaxBuilder
package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class DeclarationTests extends AbstractPassTest {
  "testImports" ignore AstFixture("") { cpg =>
    assertParse("import Foundation")

    assertParse("@_spi(Private) import SwiftUI")

    assertParse("@_exported import class Foundation.Thread")

    assertParse(#"@_private(sourceFile: "YetAnotherFile.swift") import Foundation"#)
  }

  "testStructParsing" ignore AstFixture("") { cpg =>
    assertParse("struct Foo {}")
  }

  "testFuncParsing" ignore AstFixture("") { cpg =>
    assertParse("func foo() {}")

    assertParse("func foo() -> Slice<MinimalMutableCollection<T>> {}")

    assertParse(
      """
      func onEscapingAutoclosure(_ fn: @Sendable @autoclosure @escaping () -> Int) { }
      func onEscapingAutoclosure2(_ fn: @escaping @autoclosure @Sendable () -> Int) { }
      func bar(_ : String) async -> [[String]: Array<String>] {}
      func tupleMembersFunc() -> (Type.Inner, Type2.Inner2) {}
      func myFun<S: T & U>(var1: S) {
        // do stuff
      }
      """
    )

    assertParse(
      """
      func 1️⃣where2️⃣
      r3️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "keyword 'where' cannot be used as an identifier here",
          fixIts: ["if this name is unavoidable, use backticks to escape it"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected '(' to start parameter clause", fixIts: ["insert '('"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected ':' and type in parameter", fixIts: ["insert ':' and type"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected ')' to end parameter clause", fixIts: ["insert ')'"]),
      ],
      fixedSource: """
        func `where`(
        r: <#type#>)
        """
    )

    assertParse("func /^/ (lhs: Int, rhs: Int) -> Int { 1 / 2 }")

    assertParse(
      "func /^1️⃣notoperator^/ (lhs: Int, rhs: Int) -> Int { 1 / 2 }",
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'notoperator^/' before parameter clause")
      ]
    )

    assertParse("func /^ (lhs: Int, rhs: Int) -> Int { 1 / 2 }")

    assertParse(
      """
      func name(_ default: Int) {}
      """,
      substructure: FunctionParameterSyntax(
        firstName: .wildcardToken(),
        secondName: .identifier("default"),
        colon: .colonToken(),
        type: IdentifierTypeSyntax(name: .identifier("Int"))
      )
    )

    assertParse(
      """
      class MyClass {
        1️⃣foo()
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected 'func' in function",
          fixIts: ["insert 'func'"]
        )
      ],
      fixedSource: """
        class MyClass {
          func foo()
        }
        """
    )

    assertParse(
      """
      class MyClass {
        1️⃣foo<Int>2️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected 'func' in function",
          fixIts: ["insert 'func'"]
        ),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"]),
      ],
      fixedSource: """
        class MyClass {
          func foo<Int>()
        }
        """
    )

    assertParse(
      """
      class MyClass {
        1️⃣foo
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "unexpected code 'foo' in class"
        )
      ]
    )
  }

  "testFuncAfterUnbalancedClosingBrace" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣}
      func foo() {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected brace before function")
      ]
    )
  }

  "testClassParsing" ignore AstFixture("") { cpg =>
    assertParse("class Foo {}")

    assertParse(
      """
      @dynamicMemberLookup @available(swift 4.0)
      public class MyClass {
        let A: Int
        let B: Double
      }
      """
    )

    assertParse("struct A<@NSApplicationMain T: AnyObject> {}")

    assertParse(
      "class T where t1️⃣",
      diagnostics: [
        DiagnosticSpec(message: "expected ':' or '==' to indicate a conformance or same-type requirement"),
        DiagnosticSpec(message: "expected member block in class", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        class T where t{
        }
        """
    )
    assertParse(
      "class Bℹ️<where g1️⃣",
      diagnostics: [
        DiagnosticSpec(
          message: "expected ':' or '==' to indicate a conformance or same-type requirement"
        ),
        DiagnosticSpec(
          message: "expected '>' to end generic parameter clause",
          notes: [NoteSpec(message: "to match this opening '<'")],
          fixIts: ["insert '>'"]
        ),
        DiagnosticSpec(
          message: "expected member block in class",
          fixIts: ["insert member block"]
        ),
      ],
      fixedSource: """
        class B<where g> {
        }
        """
    )
  }

  "testActorParsing" ignore AstFixture("") { cpg =>
    assertParse("actor Foo {}")

    assertParse(
      """
      actor Foo {
        nonisolated init?() {
          for (x, y, z) in self.triples {
            precondition(isSafe)
          }
        }
        subscript(_ param: String) -> Int {
          return 42
        }
      }
      """
    )
  }

  "testActorAfterUnbalancedClosingBrace" ignore AstFixture("") { cpg =>
    assertParse(
      """
      1️⃣}
      actor Foo {}
      """,
      diagnostics: [
        DiagnosticSpec(message: "unexpected brace before actor")
      ]
    )
  }

  "testNonisolatedUnsafeParsing" ignore AstFixture("") { cpg =>
    assertParse(
      """
      nonisolated(unsafe) let a = 0

      struct A {
        nonisolated(unsafe) let b = 0
        nonisolated(unsafe) var c: Int { 0 }
        nonisolated(1️⃣safe) let d = 0
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected 'unsafe' in modifier",
          fixIts: ["replace 'safe' with 'unsafe'"]
        )
      ],
      fixedSource: """
        nonisolated(unsafe) let a = 0

        struct A {
          nonisolated(unsafe) let b = 0
          nonisolated(unsafe) var c: Int { 0 }
          nonisolated(unsafe) let d = 0
        }
        """,
      experimentalFeatures: [.globalConcurrency]
    )
  }

  "testProtocolParsing" ignore AstFixture("") { cpg =>
    assertParse("protocol Foo {}")

    assertParse("protocol P { init() }")

    assertParse(
      """
      protocol P {
        associatedtype Foo: Bar where X.Y == Z.W.W.Self

        var foo: Bool { get set }
        subscript<R>(index: Int) -> R
      }
      """
    )

    assertParse(
      "protocol Pℹ️{1️⃣{}case2️⃣",
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "unexpected code '{}' before enum case"
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected identifier in enum case",
          fixIts: ["insert identifier"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected '}' to end protocol",
          notes: [NoteSpec(message: "to match this opening '{'")],
          fixIts: ["insert '}'"]
        ),
      ],
      fixedSource: """
        protocol P{{}case <#identifier#>
        }
        """
    )
  }

  "testVariableDeclarations" ignore AstFixture("") { cpg =>
    assertParse(
      """
      z

      var x: Double = z
      """
    )

    assertParse(
      """
      async let a = fetch("1.jpg")
      async let b: Image = fetch("2.jpg")
      async let secondPhotoToFetch = fetch("3.jpg")
      async let theVeryLastPhotoWeWant = fetch("4.jpg")
      """
    )

    assertParse("private unowned(unsafe) var foo: Int")
    assertParse("unowned(unsafe) let unmanagedVar: Class = c")

    assertParse("_ = foo?.description")

    assertParse(
      "_ = foo/* */?.description1️⃣",
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and expression after '? ...' in ternary expression", fixIts: ["insert ':' and expression"])
      ],
      fixedSource: """
        _ = foo/* */?.description : <#expression#>
        """
    )

    assertParse("var a = Array<Int>?(from: decoder)")

    assertParse("@Wrapper var café = 42")

    assertParse(
      """
      var x: T {
        get async {
          foo()
          bar()
        }
      }
      """
    )

    assertParse(
      """
      var foo: Int {
        _read {
          yield 1234567890
        }
        _modify {
          var someLongVariable = 0
          yield &someLongVariable
        }
      }
      """
    )

    assertParse(
      """
      var foo: Int {
        @available(swift 5.0)
        func myFun() -> Int {
          return 42
        }
        return myFun()
      }
      """
    )

    assertParse(
      """
      var foo: Int {
        mutating set {
          test += 1
        }
      }
      """
    )
  }

  "testAccessLevelModifier" ignore AstFixture("") { cpg =>
    assertParse(
      """
      open1️⃣ open(set)2️⃣ var openProp = 0
      public public(set) var publicProp = 0
      package package(set) var packageProp = 0
      internal internal(set) var internalProp = 0
      fileprivate fileprivate(set) var fileprivateProp = 0
      private private(set) var privateProp = 0
      internal(set) var defaultProp = 0
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
      ],
      applyFixIts: ["insert newline"],
      fixedSource: """
        open
        open(set)
        var openProp = 0
        public public(set) var publicProp = 0
        package package(set) var packageProp = 0
        internal internal(set) var internalProp = 0
        fileprivate fileprivate(set) var fileprivateProp = 0
        private private(set) var privateProp = 0
        internal(set) var defaultProp = 0
        """
    )

    assertParse(
      """
      open1️⃣ open(set)2️⃣ var openProp = 0
      public public(set) var publicProp = 0
      package package(set) var packageProp = 0
      internal internal(set) var internalProp = 0
      fileprivate fileprivate(set) var fileprivateProp = 0
      private private(set) var privateProp = 0
      internal(set) var defaultProp = 0
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: """
        open; open(set); var openProp = 0
        public public(set) var publicProp = 0
        package package(set) var packageProp = 0
        internal internal(set) var internalProp = 0
        fileprivate fileprivate(set) var fileprivateProp = 0
        private private(set) var privateProp = 0
        internal(set) var defaultProp = 0
        """
    )

    assertParse(
      """
      private(1️⃣get) var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'set' in modifier", fixIts: ["replace 'get' with 'set'"])
      ],
      fixedSource: """
        private(set) var a = 0
        """
    )

    assertParse(
      """
      private(1️⃣+
        set
      ) var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'set)' to end modifier", fixIts: ["insert 'set)'"]),
        // FIXME: It should print `+` as detail of text.
        DiagnosticSpec(message: "unexpected code in variable"),
      ],
      fixedSource: """
        private(set) +
          set
        ) var a = 0
        """
    )

    assertParse(
      """
      private(1️⃣get, set) var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected 'set' in modifier",
          fixIts: ["remove 'get,'"]
        )
      ],
      fixedSource: """
        private(set) var a = 0
        """
    )

    assertParse(
      """
      private(1️⃣get: set) var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected 'set' in modifier",
          fixIts: ["remove 'get:'"]
        )
      ],
      fixedSource: """
        private(set) var a = 0
        """
    )

    assertParse(
      """
      1️⃣private(
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous code 'private(' at top level")
      ]
    )

    assertParse(
      """
      private(1️⃣var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'set)' to end modifier", fixIts: ["insert 'set)'"])
      ],
      fixedSource: """
        private(set) var a = 0
        """
    )

    assertParse(
      """
      private(1️⃣get, set, didSet) var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected 'set' in modifier",
          fixIts: ["remove 'get,' and ', didSet'"]
        )
      ],
      fixedSource: """
        private(set) var a = 0
        """
    )

    assertParse(
      """
      private(1️⃣get, didSet var a = 0
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected 'set)' to end modifier", fixIts: ["insert 'set)'"]),
        DiagnosticSpec(message: "unexpected code 'get, didSet' in variable"),
      ],
      fixedSource: """
        private(set) get, didSet var a = 0
        """
    )
  }

  "testTypealias" ignore AstFixture("") { cpg =>
    assertParse("typealias Foo = Int")

    assertParse("typealias MyAlias = (_ a: Int, _ b: Double, _ c: Bool, _ d: String) -> Bool")

    assertParse("typealias A = @attr1 @attr2(hello) (Int) -> Void")
  }

  "testPrecedenceGroup" ignore AstFixture("") { cpg =>
    assertParse(
      """
      precedencegroup FooGroup {
        higherThan: Group1, Group2
        lowerThan: Group3, Group4
        associativity: left
        assignment: false
      }
      """
    )

    assertParse(
      """
      precedencegroup FunnyPrecedence {
       associativity: left
       higherThan: MultiplicationPrecedence
      }
      """
    )
  }

  "testOperators" ignore AstFixture("") { cpg =>
    assertParse("infix operator *-* : FunnyPrecedence")

    assertParse(
      """
      infix operator  <*<<< : MediumPrecedence, &
      prefix operator ^^ : PrefixMagicOperatorProtocol
      infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
      postfix operator ^^ : PostfixMagicOperatorProtocol
      infix operator ^^ : PostfixMagicOperatorProtocol, Class, Struct
      """
    )
  }

  "testObjCAttribute" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @objc(
        thisMethodHasAVeryLongName:
        foo:
        bar:
      )
      func f() {}
      """
    )
  }

  "testDifferentiableAttribute" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @differentiable(wrt: x where T: D)
      func foo<T>(_ x: T) -> T {}

      @differentiable(wrt: x where T: Differentiable)
      func foo<T>(_ x: T) -> T {}

      @differentiable(wrt: theVariableNamedX where T: Differentiable)
      func foo<T>(_ theVariableNamedX: T) -> T {}

      @differentiable(wrt: (x, y))
      func foo<T>(_ x: T) -> T {}
      """
    )
  }

  "testParsePoundError" ignore AstFixture("") { cpg =>
    assertParse(#"#error("Unsupported platform")"#)
  }

  "testParsePoundWarning" ignore AstFixture("") { cpg =>
    assertParse(#"#warning("Unsupported platform")"#)
  }

  "testParseSpecializeAttribute" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      @_specialize(where T == Int, U == Float)
      mutating func exchangeSecond<U>(_ u: U, _ t: T) -> (U, T) {
        x = t
        return (u, x)
      }

      @_specialize(exported: true, kind: full, where K == Int, V == Int)
      @_specialize(exported: false, kind: partial, where K: _Trivial64)
      func dictFunction<K, V>(dict: Dictionary<K, V>) {
      }

      @_specialize(where T == Int)
      public func play() {
        for _ in 0...100_000_000 { t = t.ping() }
      }

      @_specialize(exported: true,
                   spi: SwiftSpecialization,
                   target: copy(),
                   where Key == AnyHashable, Value == Any)
      @_specialize(exported: true,
                   spi: SwiftSpecialization,
                   target: copy(),
                   where Key == AnyHashable, Value == String)
      @_specialize(exported: true,
                   spi: SwiftSpecialization,
                   target: copy(),
                   where Key == String, Value == Any)
      @_specialize(exported: true,
                   spi: SwiftSpecialization,
                   target: copy(),
                   where Key == String, Value == AnyHashable)
      @_specialize(exported: true,
                   spi: SwiftSpecialization,
                   target: copy(),
                   where Key == String, Value == String)
      @available(SwiftStdlib 5.5, *)
      @usableFromInline
      mutating func __specialize_copy() { Builtin.unreachable() }

      @_specializeExtension
      extension Sequence {
        @_specialize(exported: true,
                     spi: SwiftSpecialization,
                     target: _copyContents(initializing:),
                     where Self == [String])
        @_specialize(exported: true,
                     spi: SwiftSpecialization,
                     target: _copyContents(initializing:),
                     where Self == Set<String>)
        @available(SwiftStdlib 5.5, *)
        @usableFromInline
        __consuming func __specialize__copyContents(initializing: Swift.UnsafeMutableBufferPointer<Element>)  -> (Iterator, Int) { Builtin.unreachable() }
      }
      """#
    )

    assertParse(
      """
      @_specialize(where T: _Trivial(32), T: _Trivial(64), T: _Trivial, T: _RefCountedObject)
      @_specialize(where T: _Trivial, T: _Trivial(64))
      @_specialize(where T: _RefCountedObject, T: _NativeRefCountedObject)
      @_specialize(where Array<T> == Int)
      @_specialize(where T.Element == Int)
      public func funcWithComplexSpecializeRequirements<T: ProtocolWithDep>(t: T) -> Int {
        return 55555
      }
      """
    )
  }

  "testParseRetroactiveExtension" ignore AstFixture("") { cpg =>
    assertParse(
      """
      extension Int: @retroactive Identifiable {}
      """
    )
  }

  "testParseDynamicReplacement" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @_dynamicReplacement(for: dynamic_replaceable())
      func replacement() {
        dynamic_replaceable()
      }
      """
    )

    assertParse(
      """
      @_dynamicReplacement(for: subscript(_:))
      subscript(x y: Int) -> Int {
        get {
          return self[y]
        }
        set {
          self[y] = newValue
        }
      }
      """
    )

    assertParse(
      """
      @_dynamicReplacement(for: dynamic_replaceable_var)
      var r : Int {
        return 0
      }
      """
    )

    assertParse(
      """
      @_dynamicReplacement(for: init(x:))
      init(y: Int) {
        self.init(x: y + 1)
      }
      """
    )
  }

  "testEnumParsing" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Foo {
        @preconcurrency case custom(@Sendable () throws -> Void)
      }
      """
    )

    assertParse(
      """
      enum Content {
        case keyPath(KeyPath<FocusedValues, Value?>)
        case keyPath(KeyPath<FocusedValues, Binding<Value>?>)
        case value(Value?)
      }
      """
    )
  }

  "testStandaloneModifier" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct a {
        public1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected declaration after 'public' modifier", fixIts: ["insert declaration"])
      ],
      fixedSource: """
        struct a {
          public <#declaration#>
        }
        """
    )
  }

  "testEditorPlaceholderWithModifier" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct a {
        public1️⃣<#declaration#>
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "editor placeholder in source file")
      ]
    )
  }

  "testMissingColonInFunctionSignature" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(first second 1️⃣Int)",
      diagnostics: [
        DiagnosticSpec(message: "expected ':' in parameter", fixIts: ["insert ':'"])
      ],
      fixedSource: """
        func test(first second: Int)
        """
    )
  }

  "testExtraArgumentLabelsInFunctionSignature" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(first second 1️⃣third fourth: Int)",
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'third fourth' in parameter")
      ]
    )
  }

  "testMissingClosingParenInFunctionSignature" ignore AstFixture("") { cpg =>
    assertParse(
      "func testℹ️(first second: Int1️⃣",
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' to end parameter clause",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: """
        func test(first second: Int)
        """
    )
  }

  "testMissingOpeningParenInFunctionSignature" ignore AstFixture("") { cpg =>
    assertParse(
      "func test 1️⃣first second: Int)",
      diagnostics: [
        DiagnosticSpec(message: "expected '(' to start parameter clause", fixIts: ["insert '('"])
      ],
      fixedSource: """
        func test(first second: Int)
        """
    )
  }

  "testNoParamsForFunction" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class MyClass {
        func withoutParameters1️⃣

        func withParameters() {}
      }
      """,
      substructure: FunctionDeclSyntax(
        funcKeyword: .keyword(.func),
        name: .identifier("withoutParameters"),
        signature: FunctionSignatureSyntax(
          parameterClause: FunctionParameterClauseSyntax(
            leftParen: .leftParenToken(presence: .missing),
            parameters: FunctionParameterListSyntax([]),
            rightParen: .rightParenToken(presence: .missing)
          )
        )
      ),
      diagnostics: [
        DiagnosticSpec(message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        class MyClass {
          func withoutParameters()

          func withParameters() {}
        }
        """
    )
  }

  "testThrowsInWrongLocation" ignore AstFixture("") { cpg =>
    assertParse(
      "func test() -> 1️⃣throws Int",
      diagnostics: [
        DiagnosticSpec(message: "'throws' must precede '->'", fixIts: ["move 'throws' in front of '->'"])
      ],
      fixedSource: "func test() throws -> Int"
    )
  }

  "testTypedThrows" ignore AstFixture("") { cpg =>
    assertParse(
      "func test() throws(any Error) -> Int { }",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      """
      struct X {
        init() throws(any Error) { }
      }
      """,
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws(MyError) 1️⃣async {}",
      diagnostics: [
        DiagnosticSpec(message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"])
      ],
      fixedSource: "func test() async throws(MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws 1️⃣async2️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() async throws (MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣try2️⃣(MyError) async {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError) async' in function"),
      ],
      fixedSource: "func test() throws (MyError) async {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣try 2️⃣async3️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() async throws (MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws(MyError) 1️⃣await {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'await' must precede 'throws'", fixIts: ["move 'await' in front of 'throws'"])
      ],
      fixedSource: "func test() async throws(MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws 1️⃣await2️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "'await' must precede 'throws'", fixIts: ["move 'await' in front of 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() async throws (MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣try2️⃣(MyError) await {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError) await' in function"),
      ],
      fixedSource: "func test() throws (MyError) await {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣try await2️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() awaitthrows (MyError) {}",  // FIXME: spacing
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() async1️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '(MyError)' in function")
      ],
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣await2️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'await' with 'async'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() async (MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() 1️⃣try2️⃣(MyError) {}",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected throwing specifier; did you mean 'throws'?", fixIts: ["replace 'try' with 'throws'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code '(MyError)' in function"),
      ],
      fixedSource: "func test() throws (MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws(MyError) {}",
      experimentalFeatures: [.typedThrows]
    )

    assertParse(
      "func test() throws(MyError)1️⃣async {}",  // no space between closing parenthesis and `async`
      diagnostics: [
        DiagnosticSpec(message: "'async' must precede 'throws'", fixIts: ["move 'async' in front of 'throws'"])
      ],
      fixedSource: "func test() async throws(MyError){}",
      experimentalFeatures: [.typedThrows]
    )
  }

  "testExtraneousRightBraceRecovery" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class ABC {
        let def = ghi(jkl: mno)
      }
      1️⃣}
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous brace at top level")
      ]
    )
  }

  "testMissingSubscriptReturnClause" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        subscript(x: String) 1️⃣{}
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"])
      ],
      fixedSource: """
        struct Foo {
          subscript(x: String) -> <#type#> {}
        }
        """
    )
  }

  "testClassWithLeadingNumber" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class 1️⃣23class {
        func 2️⃣24method() {}
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "identifier can only start with a letter or underscore, not a number"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "identifier can only start with a letter or underscore, not a number"),
      ]
    )
  }

  "testAccessors" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var bad1 : Int {
        _read async { 0 }
      }
      """
    )

    assertParse(
      """
      var bad2 : Int {
        get 1️⃣reasync { 0 }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected async specifier; did you mean 'async'?", fixIts: ["replace 'reasync' with 'async'"])
      ],
      fixedSource: """
        var bad2 : Int {
          get async { 0 }
        }
        """
    )
  }

  "testInitAccessor" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S {
        var value: Int {
          init {}
          get {}
          set {}
        }
      }
      """
    )

    assertParse(
      """
      struct S {
        let _value: Int

        init() {
        }
      }
      """
    )

    assertParse(
      """
      struct S {
        var value: Int {
          init(newValue) {}
          get {}
          set(newValue) {}
        }
      }
      """
    )

    assertParse(
      """
      struct S {
        var value: Int {
          init(newValue) {}
          get {}
          set(newValue) {}
        }
      }
      """
    )
  }

  "testExpressionMember" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S 1️⃣{2️⃣
        3️⃣/4️⃣ ###line 25 "line-directive.swift"5️⃣
      6️⃣}
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected '}' to end struct",
          notes: [NoteSpec(locationMarker: "1️⃣", message: "to match this opening '{'")],
          fixIts: ["insert '}'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "bare slash regex literal may not start with space"
        ),
        DiagnosticSpec(
          locationMarker: "5️⃣",
          message: "expected '/' to end regex literal",
          notes: [NoteSpec(locationMarker: "3️⃣", message: "to match this opening '/'")],
          fixIts: ["insert '/\'"]
        ),
        DiagnosticSpec(
          locationMarker: "6️⃣",
          message: "extraneous brace at top level"
        ),
      ],
      fixedSource: """
        struct S {
        }
          / ###line 25 "line-directive.swift"/
        }
        """
    )
  }

  "testBogusProtocolRequirements" ignore AstFixture("") { cpg =>
    assertParse(
      """
      protocol P {
        var prop : Int { get 1️⃣bogus rethrows set }
      }
      """,
      substructure: AccessorBlockSyntax(
        accessors: .accessors(
          AccessorDeclListSyntax([
            AccessorDeclSyntax(
              accessorSpecifier: .keyword(.get)
            )
          ])
        ),
        UnexpectedNodesSyntax([
          TokenSyntax.identifier("bogus"), TokenSyntax.keyword(.rethrows),
          TokenSyntax.identifier("set"),
        ])
      ),
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'bogus rethrows set' in variable")
      ]
    )
  }

  "testTextRecovery1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      Lorem1️⃣ ipsum2️⃣ dolor3️⃣ sit4️⃣ amet5️⃣, consectetur adipiscing elit
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(locationMarker: "5️⃣", message: "extraneous code ', consectetur adipiscing elit' at top level"),
      ],
      applyFixIts: ["insert newline"],
      fixedSource: """
        Lorem
        ipsum
        dolor
        sit
        amet, consectetur adipiscing elit
        """
    )
  }

  "testTextRecovery2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      Lorem1️⃣ ipsum2️⃣ dolor3️⃣ sit4️⃣ amet5️⃣, consectetur adipiscing elit
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        ),
        DiagnosticSpec(locationMarker: "5️⃣", message: "extraneous code ', consectetur adipiscing elit' at top level"),
      ],
      applyFixIts: ["insert ';'"],
      fixedSource: """
        Lorem; ipsum; dolor; sit; amet, consectetur adipiscing elit
        """
    )
  }

  "testRecoverOneExtraLabel" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(first second 1️⃣third: Int)",
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        UnexpectedNodesSyntax([TokenSyntax.identifier("third")]),
        colon: .colonToken(),
        type: IdentifierTypeSyntax(name: .identifier("Int"))
      ),
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'third' in parameter")
      ]
    )
  }

  "testRecoverTwoExtraLabels" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(first second 1️⃣third fourth: Int)",
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        UnexpectedNodesSyntax([TokenSyntax.identifier("third"), TokenSyntax.identifier("fourth")]),
        colon: .colonToken(),
        type: IdentifierTypeSyntax(name: .identifier("Int"))
      ),
      diagnostics: [
        DiagnosticSpec(message: "unexpected code 'third fourth' in parameter")
      ]
    )
  }

  "testDontRecoverFromDeclKeyword" ignore AstFixture("") { cpg =>
    assertParse(
      "func fooℹ️(first second 1️⃣third 2️⃣struct3️⃣: Int4️⃣) {}",
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        colon: .colonToken(presence: .missing),
        type: IdentifierTypeSyntax(name: .identifier("third"))
      ),
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected ':' in parameter",
          fixIts: ["insert ':'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected ')' to end parameter clause",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "expected identifier in struct",
          fixIts: ["insert identifier"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "unexpected code ')' in struct"
        ),
      ],
      fixedSource: """
        func foo(first second: third)struct <#identifier#>: Int) {}
        """
    )
  }

  "testRecoverFromParens" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(first second 1️⃣[third fourth]: Int)",
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        UnexpectedNodesSyntax([
          TokenSyntax.leftSquareToken(),
          TokenSyntax.identifier("third"),
          TokenSyntax.identifier("fourth"),
          TokenSyntax.rightSquareToken(),
        ]),
        colon: .colonToken(),
        type: IdentifierTypeSyntax(name: .identifier("Int"))
      ),
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '[third fourth]' in parameter")
      ]
    )
  }

  "testDontRecoverFromUnbalancedParens" ignore AstFixture("") { cpg =>
    assertParse(
      "func foo(first second 1️⃣[third 2️⃣fourth: Int) {}",
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        colon: .colonToken(presence: .missing),
        type: ArrayTypeSyntax(
          leftSquare: .leftSquareToken(),
          element: IdentifierTypeSyntax(name: .identifier("third")),
          rightSquare: .rightSquareToken(presence: .missing)
        )
      ),
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected ':' in parameter",
          fixIts: ["insert ':'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected ']' to end array type",
          notes: [NoteSpec(locationMarker: "1️⃣", message: "to match this opening '['")],
          fixIts: ["insert ']'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "unexpected code 'fourth: Int' in parameter clause"
        ),
      ],
      fixedSource: """
        func foo(first second: [third]fourth: Int) {}
        """
    )
  }

  "testDontRecoverIfNewlineIsBeforeColon" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func fooℹ️(first second 1️⃣third2️⃣
      3️⃣: Int) {}
      """,
      substructure: FunctionParameterSyntax(
        firstName: .identifier("first"),
        secondName: .identifier("second"),
        colon: .colonToken(presence: .missing),
        type: IdentifierTypeSyntax(name: .identifier("third"))
      ),
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected ':' in parameter",
          fixIts: ["insert ':'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected ')' to end parameter clause",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "extraneous code ': Int) {}' at top level"
        ),
      ],
      fixedSource: """
        func foo(first second: third)
        : Int) {}
        """
    )
  }

  "testMalforedStruct" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct n1️⃣
      #if2️⃣
      @3️⃣
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '{' in struct", fixIts: ["insert '{'"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected condition in conditional compilation clause", fixIts: ["insert condition"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected name in attribute", fixIts: ["insert name"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected declaration after attribute", fixIts: ["insert declaration"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected '#endif' in conditional compilation block", fixIts: ["insert '#endif'"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected '}' to end struct", fixIts: ["insert '}'"]),
      ],
      fixedSource: """
        struct n {
        #if <#expression#>
        @<#type#> <#declaration#>
        #endif
        }
        """
    )
  }

  "testInitializers" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct S0 {
        init!(int: Int) { }
        init! (uint: UInt) { }
        init !(float: Float) { }

        init?(string: String) { }
        init ?(double: Double) { }
        init ? (char: Character) { }
      }
      """
    )
  }

  "testDeinitializers" ignore AstFixture("") { cpg =>
    assertParse("deinit {}")
    assertParse("deinit")
  }

  "testAttributedMember" ignore AstFixture("") { cpg =>
    assertParse(
      #"""
      struct Foo {
        @Argument(help: "xxx")
        var generatedPath: String
      }
      """#
    )
  }

  "testAnyAsParameterLabel" ignore AstFixture("") { cpg =>
    assertParse(
      "func at(any kinds: [RawTokenKind]) {}"
    )
  }

  "testPublicClass" ignore AstFixture("") { cpg =>
    assertParse("public class Foo: Superclass {}")
  }

  "testReturnVariableNamedAsync" ignore AstFixture("") { cpg =>
    assertParse(
      ##"""
      if let async = self.consume(if: .keyword(.async)) {
        return async
      }

      if let reasync = self.consume(if: .keyword(.reasync)) {
        return reasync
      }
      """##
    )
  }

  "testLeadingUnexpectedTokens" ignore AstFixture("") { cpg =>
    assertParse(
      "1️⃣}class C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before class"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected member block in class", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        }class C {
        }
        """
    )
    assertParse(
      "1️⃣}enum C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before enum"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected member block in enum", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        }enum C {
        }
        """
    )
    assertParse(
      "1️⃣}protocol C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before protocol"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected member block in protocol", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        }protocol C {
        }
        """
    )
    assertParse(
      "1️⃣}actor C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before actor"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected member block in actor", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        }actor C {
        }
        """
    )
    assertParse(
      "1️⃣}struct C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before struct"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected member block in struct", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        }struct C {
        }
        """
    )
    assertParse(
      "1️⃣}func C2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before function"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"]),
      ],
      fixedSource: """
        }func C()
        """
    )
    assertParse(
      "1️⃣}init2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before initializer"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"]),
      ],
      fixedSource: """
        }init()
        """
    )
    assertParse(
      "1️⃣}subscript2️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected brace before subscript"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected parameter clause in subscript", fixIts: ["insert parameter clause"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected '->' and return type in subscript", fixIts: ["insert '->' and return type"]),
      ],
      fixedSource: """
        }subscript() -> <#type#>
        """
    )
  }

  "testBogusTypeDeclName" ignore AstFixture("") { cpg =>
    assertParse(
      "associatedtype 1️⃣5s",
      diagnostics: [
        DiagnosticSpec(message: "identifier can only start with a letter or underscore, not a number")
      ]
    )
  }

  "testModifiedParameter" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func const(_const _ map: String) {}
      """
    )

    assertParse(
      """
      func isolated(isolated _ 1️⃣map: String) {}
      """
    )

    assertParse(
      """
      func isolatedConst(isolated _const _ 1️⃣map: String) {}
      """
    )

    assertParse(
      """
      func nonEphemeralIsolatedConst(@_nonEmphemeral isolated _const _ 1️⃣map: String) {}
      """
    )

    assertParse(
      """
      func const(_const map: String) {}
      """
    )

    assertParse(
      """
      func isolated(isolated map: String) {}
      """
    )

    assertParse(
      """
      func isolatedConst(isolated _const map1️⃣: String) {}
      """
    )

    assertParse(
      """
      func const(_const x: String) {}
      func isolated(isolated: String) {}
      func isolatedConst(isolated _const: String) {}
      """
    )
  }

  "testReasyncFunctions" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class MyType {
        init(_ f: () async -> Void) reasync {
          await f()
        }

        func foo(index: Int) reasync rethrows -> String {
          await f()
        }
      }
      """
    )
  }

  "testStandaloneAtSignInGenericParameter" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct U1️⃣<@2️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected name in attribute",
          fixIts: ["insert name"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected name in generic parameter",
          fixIts: ["insert name"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected '>' to end generic parameter clause",
          notes: [NoteSpec(locationMarker: "1️⃣", message: "to match this opening '<'")],
          fixIts: ["insert '>'"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected member block in struct",
          fixIts: ["insert member block"]
        ),
      ],
      fixedSource: """
        struct U<@<#type#> <#identifier#>> {
        }
        """
    )
  }

  "testMatchBracesBasedOnSpaces" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        struct Bar 1️⃣
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected member block in struct", fixIts: ["insert member block"])
      ],
      fixedSource: """
        struct Foo {
          struct Bar {
        }
        }
        """
    )
  }

  "testFunctionWithoutNameOrArguments" ignore AstFixture("") { cpg =>
    assertParse(
      "func 1️⃣{}",
      diagnostics: [
        DiagnosticSpec(message: "expected identifier and function signature in function", fixIts: ["insert identifier and function signature"])
      ],
      fixedSource: """
        func <#identifier#>() {}
        """
    )
  }

  "testMacroExpansionDeclaration" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X {
        #memberwiseInit(access: .public)
      }
      """
    )
    assertParse(
      """
      #expand
      """,
      substructure: SourceFileSyntax(
        CodeBlockItemListSyntax {
          MacroExpansionDeclSyntax(macroName: "expand") {}
        }
      )
    )
  }

  "testMacroExpansionDeclarationWithKeywordName" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct X {
        #case
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        pound: .poundToken(),
        macroName: .identifier("case"),
        arguments: LabeledExprListSyntax([])
      )
    )
  }

  "testAttributedMacroExpansionDeclaration" ignore AstFixture("") { cpg =>
    assertParse(
      """
      @attribute #topLevelWithAttr
      """,
      substructure: MacroExpansionDeclSyntax(
        attributes: [.attribute(AttributeSyntax(attributeName: TypeSyntax("attribute")))],
        macroName: "topLevelWithAttr",
        arguments: []
      )
    )

    assertParse(
      """
      public #topLevelWithModifier
      """,
      substructure: MacroExpansionDeclSyntax(
        modifiers: [DeclModifierSyntax(name: .keyword(.public))],
        macroName: "topLevelWithModifier",
        arguments: []
      )
    )

    assertParse(
      """
      #topLevelBare
      """,
      substructure: MacroExpansionExprSyntax(
        macroName: "topLevelBare",
        arguments: []
      )
    )

    assertParse(
      """
      struct S {
        @attribute #memberWithAttr
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        attributes: [.attribute(AttributeSyntax(attributeName: TypeSyntax("attribute")))],
        macroName: "memberWithAttr",
        arguments: []
      )
    )

    assertParse(
      """
      struct S {
        public #memberWithModifier
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        modifiers: [DeclModifierSyntax(name: .keyword(.public))],
        macroName: "memberWithModifier",
        arguments: []
      )
    )

    assertParse(
      """
      struct S {
        #memberBare
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        macroName: "memberBare",
        arguments: []
      )
    )

    assertParse(
      """
      func test() {
        @attribute #bodyWithAttr
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        attributes: [.attribute(AttributeSyntax(attributeName: TypeSyntax("attribute")))],
        macroName: "bodyWithAttr",
        arguments: []
      )
    )

    assertParse(
      """
      func test() {
        public #bodyWithModifier
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        modifiers: [DeclModifierSyntax(name: .keyword(.public))],
        macroName: "bodyWithModifier",
        arguments: []

      )
    )

    assertParse(
      """
      func test() {
        #bodyBare
      }
      """,
      substructure: MacroExpansionExprSyntax(
        macroName: "bodyBare",
        arguments: []
      )
    )

    assertParse(
      """
      func test() {
        @attrib1
        @attrib2
        public
        #declMacro
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        attributes: [
          .attribute(AttributeSyntax(attributeName: TypeSyntax("attrib1"))),
          .attribute(AttributeSyntax(attributeName: TypeSyntax("attrib2"))),
        ],
        modifiers: [DeclModifierSyntax(name: .keyword(.public))],
        macroName: "declMacro",
        arguments: []
      )
    )

    assertParse(
      """
      struct S {
        @attrib #class
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        attributes: [.attribute(AttributeSyntax(attributeName: TypeSyntax("attrib")))],
        pound: .poundToken(),
        macroName: .identifier("class"),
        arguments: []
      )
    )

    assertParse(
      """
      struct S {
        #struct
      }
      """,
      substructure: MacroExpansionDeclSyntax(
        pound: .poundToken(),
        macroName: .identifier("struct"),
        arguments: []
      )
    )
  }

  "testWhitespaceBetweenPoundAndMacroName" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #1️⃣ myMacroName
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous whitespace after '#' is not permitted", fixIts: ["remove whitespace"])
      ],
      fixedSource: """
        #myMacroName
        """
    )

    assertParse(
      """
      #1️⃣ /*comment*/ myMacroName
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous whitespace after '#' is not permitted", fixIts: ["remove whitespace"])
      ],
      fixedSource: """
        #myMacroName
        """
    )

    assertParse(
      """
      #1️⃣
      myMacroName
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in macro expansion", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        #<#identifier#>
        myMacroName
        """
    )

    assertParse(
      """
      struct Foo {
        #1️⃣ myMacroName
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "extraneous whitespace after '#' is not permitted", fixIts: ["remove whitespace"])
      ],
      fixedSource: """
        struct Foo {
          #myMacroName
        }
        """
    )

    assertParse(
      """
      struct Foo {
        #1️⃣
        2️⃣myMacroName
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected identifier in macro expansion", fixIts: ["insert identifier"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code 'myMacroName' in struct"),
      ],
      fixedSource: """
        struct Foo {
          #<#identifier#>
          myMacroName
        }
        """
    )
  }

  "testVariableDeclWithGetSetButNoBrace" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        var x: Int 1️⃣
          get {
            4
          }
          set {
            x = newValue
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '{' in variable", fixIts: ["insert '{'"])
      ],
      fixedSource: """
        struct Foo {
          var x: Int {
            get {
              4
            }
            set {
              x = newValue
            }
          }
        }
        """
    )
  }

  "testVariableDeclWithSetGetButNoBrace" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        var x: Int 1️⃣
          set {
            x = newValue
          }
          get {
            4
          }
        }
      }
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected '{' in variable", fixIts: ["insert '{'"])
      ],
      fixedSource: """
        struct Foo {
          var x: Int {
            set {
              x = newValue
            }
            get {
              4
            }
          }
        }
        """
    )
  }

  "testVariableFollowedByReferenceToSet" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func bar() {
          let a = b
          set.c
      }
      """
    )
  }

  "testNominalDeclarationRecovery" ignore AstFixture("") { cpg =>
    assertParse(
      "protocol 1️⃣{}",
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in protocol", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        protocol <#identifier#> {}
        """
    )
    assertParse(
      "class 1️⃣{}",
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in class", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        class <#identifier#> {}
        """
    )
    assertParse(
      "struct 1️⃣{}",
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in struct", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        struct <#identifier#> {}
        """
    )
    assertParse(
      "enum 1️⃣{}",
      diagnostics: [
        DiagnosticSpec(message: "expected identifier in enum", fixIts: ["insert identifier"])
      ],
      fixedSource: """
        enum <#identifier#> {}
        """
    )
    // `actor` cannot recover from a missing identifier since it's contextual
    // based on the presence of the identifier.
  }

  "testDontNestClassesIfTheyContainUnexpectedTokens" ignore AstFixture("") { cpg =>
    // There used to be a bug where `class B` was parsed as a nested class of A
    // because recovery to the `class` keyword of B consumed the closing brace.
    assertParse(
      """
      class A {
        1️⃣^
      }
      class B {
      }
      """,
      substructure: CodeBlockItemListSyntax([
        CodeBlockItemSyntax(
          item: .init(
            ClassDeclSyntax(
              classKeyword: .keyword(.class),
              name: .identifier("A"),
              memberBlock: MemberBlockSyntax(
                leftBrace: .leftBraceToken(),
                members: MemberBlockItemListSyntax(),
                UnexpectedNodesSyntax([TokenSyntax.binaryOperator("^")]),
                rightBrace: .rightBraceToken()
              )
            )
          )
        ),
        CodeBlockItemSyntax(
          item: .init(
            ClassDeclSyntax(
              classKeyword: .keyword(.class),
              name: .identifier("B"),
              memberBlock: MemberBlockSyntax(
                leftBrace: .leftBraceToken(),
                members: MemberBlockItemListSyntax(),
                rightBrace: .rightBraceToken()
              )
            )
          )
        ),
      ]
      ),
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code '^' in class")
      ]
    )
  }

  "testIssue1025" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Math {
        public static let pi = 3.14
        @available(*, unavailable) init() {}
      }
      """
    )

    assertParse("func foo(body: (isolated String) -> Int) {}")
  }

  "testMacroDecl" ignore AstFixture("") { cpg =>
    assertParse(
      """
      macro m11️⃣: Int = A.M1
      macro m2(_: Int) = A.M2
      macro m3(a b: Int) -> Int = A.M3
      macro m4<T>2️⃣: T = A.M4 where T.Assoc: P
      macro m5<T: P>(_: T)
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"]),
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code ': Int = A.M1' before macro"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "unexpected code ': T = A.M4 where T.Assoc: P' before macro"),
      ],
      fixedSource: """
        macro m1(): Int = A.M1
        macro m2(_: Int) = A.M2
        macro m3(a b: Int) -> Int = A.M3
        macro m4<T>(): T = A.M4 where T.Assoc: P
        macro m5<T: P>(_: T)
        """
    )

    assertParse(
      """
      macro m1 1️⃣= A
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected parameter clause in function signature", fixIts: ["insert parameter clause"])
      ],
      fixedSource: """
        macro m1() = A
        """
    )
  }

  "testPrimaryAssociatedTypeNotTerminatedWithAngleBracket" ignore AstFixture("") { cpg =>
    assertParse(
      "protocol1️⃣<2️⃣:3️⃣",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected identifier in protocol", fixIts: ["insert identifier"]),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected name and '>' to end primary associated type clause", fixIts: ["insert name and '>'"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected type in inherited type", fixIts: ["insert type"]),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected member block in protocol", fixIts: ["insert member block"]),
      ],
      fixedSource: """
        protocol <#identifier#><<#identifier#>>: <#type#> {
        }
        """
    )
  }

  "testClassWithPrivateSet" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Properties {
        class private(set) var privateSetterCustomNames: Bool
      }
      """
    )
  }

  "testEditorPlaceholderInMemberDeclList" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class Foo {
        1️⃣<#code#>
      }
      """,
      substructure: MemberBlockItemSyntax(decl: MissingDeclSyntax(placeholder: .identifier("<#code#>"))),
      diagnostics: [
        DiagnosticSpec(message: "editor placeholder in source file")
      ]
    )
  }

  "testAttributeInPoundIf" ignore AstFixture("") { cpg =>
    assertParse(
      """
      #if hasAttribute(foo)
      @foo
      #endif
      struct MyStruct {}
      """
    )
  }

  "testMissingExpressionInVariableAssignment" ignore AstFixture("") { cpg =>
    assertParse(
      """
      let a =1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(message: "expected expression in variable", fixIts: ["insert expression"])
      ],
      fixedSource: """
        let a = <#expression#>
        """
    )
  }

  "testCallToOpenThatLooksLikeDeclarationModifier" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func test() {
        open(set)
        var foo = 2
      }
      """,
      substructure: FunctionCallExprSyntax(
        calledExpression: DeclReferenceExprSyntax(baseName: .identifier("open")),
        leftParen: .leftParenToken(),
        arguments: LabeledExprListSyntax([
          LabeledExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("set"))
          )
        ]),
        rightParen: .rightParenToken()
      )
    )
  }

  "testReferenceToOpenThatLooksLikeDeclarationModifier" ignore AstFixture("") { cpg =>
    // Ideally, this should be parsed as an identifier expression to 'open',
    // followed by a variable declaration but the current behavior matches the C++ parser.
    assertParse(
      """
      func test() {
        open
        var foo = 2
      }
      """,
      substructure: VariableDeclSyntax(
        modifiers: DeclModifierListSyntax([
          DeclModifierSyntax(name: .keyword(.open))
        ]),
        bindingSpecifier: .keyword(.var),
        bindings: PatternBindingListSyntax([
          PatternBindingSyntax(
            pattern: IdentifierPatternSyntax(identifier: .identifier("foo")),
            initializer: InitializerClauseSyntax(
              value: IntegerLiteralExprSyntax(literal: .integerLiteral("2"))
            )
          )
        ])
      )
    )
  }

  "testOpenVarInCodeBlockItemList" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func test() {
        open var foo = 2
      }
      """,
      substructure: DeclModifierSyntax(name: .keyword(.open))
    )
  }

  "testAsyncLetInLocalContext" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func foo() async {
        async let x: String = "x"
      }
      """
    )
  }

  "testBorrowingConsumingParameterSpecifiers" ignore AstFixture("") { cpg =>
    assertParse("struct borrowing {}")
    assertParse("struct consuming {}")

    assertParse("struct Foo {}")

    assertParse("func foo(x: borrowing Foo) {}")
    assertParse("func bar(x: consuming Foo) {}")
    assertParse("func baz(x: (borrowing Foo, consuming Foo) -> ()) {}")

    // `borrowing` and `consuming` are contextual keywords, so they should also
    // continue working as type and/or parameter names

    assertParse("func zim(x: borrowing) {}")
    assertParse("func zang(x: consuming) {}")
    assertParse("func zung(x: borrowing consuming) {}")
    assertParse("func zip(x: consuming borrowing) {}")
    assertParse("func zap(x: (borrowing, consuming) -> ()) {}")
    assertParse("func zoop(x: (borrowing consuming, consuming borrowing) -> ()) {}")

    // Parameter specifier names are regular identifiers in other positions,
    // including argument labels.

    assertParse("func argumentLabelOnly(borrowing: Int) {}")
    assertParse("func argumentLabelOnly(consuming: Int) {}")
    assertParse("func argumentLabelOnly(__shared: Int) {}")
    assertParse("func argumentLabelOnly(__owned: Int) {}")

    assertParse("func argumentLabel(borrowing consuming: Int) {}")
    assertParse("func argumentLabel(consuming borrowing: Int) {}")
    assertParse("func argumentLabel(__shared __owned: Int) {}")
    assertParse("func argumentLabel(__owned __shared: Int) {}")

    // We should parse them as argument labels in function types, even though that
    // isn't currently supported.

    assertParse("func argumentLabel(anonBorrowingInClosure: (_ borrowing: Int) -> ()) {}")
    assertParse("func argumentLabel(anonConsumingInClosure: (_ consuming: Int) -> ()) {}")
    assertParse("func argumentLabel(anonSharedInClosure: (_ __shared: Int) -> ()) {}")
    assertParse("func argumentLabel(anonOwnedInClosure: (_ __owned: Int) -> ()) {}")
  }

  "testMisplaceSpecifierInTupleTypeBody" ignore AstFixture("") { cpg =>
    assertParse(
      "func test(a: (1️⃣borrowing F 2️⃣o))",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code 'borrowing' in tuple type"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "expected ',' in tuple type", fixIts: ["insert ','"]),
      ],
      fixedSource: "func test(a: (borrowing F, o))"
    )
  }

  "testWhereClauseWithFunctionType" ignore AstFixture("") { cpg =>
    // A function type in the where clause isn't semantically valid but its fine
    // with the parser. Make sure we don't recover to the arrow to parse the
    // function return type.
    assertParse(
      """
      func badTypeConformance3<T>(_: T) where (T) -> () : EqualComparable { }
      """
    )
  }

  "testSuppressedImplicitConformance" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Hello: ~Copyable {}
      """,
      substructure: InheritedTypeSyntax(
        type: SuppressedTypeSyntax(
          withoutTilde: .prefixOperator("~"),
          type: TypeSyntax(stringLiteral: "Copyable")
        )
      )
    )

    assertParse(
      """
      let _: any ~Copyable = 0
      """,
      substructure: SomeOrAnyTypeSyntax(
        someOrAnySpecifier: .keyword(.any),
        constraint: SuppressedTypeSyntax(
          withoutTilde: .prefixOperator("~"),
          type: TypeSyntax(stringLiteral: "Copyable")
        )
      )
    )

    assertParse(
      """
      typealias Z = ~Copyable.Type
      """,
      substructure: SuppressedTypeSyntax(
        withoutTilde: .prefixOperator("~"),
        type: MetatypeTypeSyntax(
          baseType: TypeSyntax(stringLiteral: "Copyable"),
          metatypeSpecifier: .keyword(.Type)
        )
      )
    )

    assertParse(
      """
      typealias Z = ~A.B.C
      """,
      substructure: SuppressedTypeSyntax(
        withoutTilde: .prefixOperator("~"),
        type: MemberTypeSyntax(
          baseType: MemberTypeSyntax(
            baseType: TypeSyntax(stringLiteral: "A"),
            name: .identifier("B")
          ),
          name: .identifier("C")
        )
      )
    )

    assertParse(
      """
      typealias Z = ~A?
      """,
      substructure: SuppressedTypeSyntax(
        withoutTilde: .prefixOperator("~"),
        type: OptionalTypeSyntax(
          wrappedType: IdentifierTypeSyntax(name: .identifier("A"))
        )
      )
    )

    assertParse(
      """
      typealias Z = ~A<T>
      """,
      substructure: SuppressedTypeSyntax(
        withoutTilde: .prefixOperator("~"),
        type: IdentifierTypeSyntax(
          name: .identifier("A"),
          genericArgumentClause: GenericArgumentClauseSyntax(
            arguments: GenericArgumentListSyntax([
              GenericArgumentSyntax(
                argument:
                  IdentifierTypeSyntax(name: .identifier("T"))
              )
            ])
          )
        )
      )
    )

    assertParse(
      """
      struct Hello<T: ~Copyable> {}
      """,
      substructure: GenericParameterListSyntax([
        GenericParameterSyntax(
          attributes: AttributeListSyntax([]),
          name: .identifier("T"),
          colon: .colonToken(),
          inheritedType: SuppressedTypeSyntax(
            withoutTilde: .prefixOperator("~"),
            type: TypeSyntax(stringLiteral: "Copyable")
          )
        )
      ])
    )

    assertParse(
      """
      func henlo<T: ~Copyable>(_ t: T) {}
      """,
      substructure: GenericParameterListSyntax([
        GenericParameterSyntax(
          attributes: AttributeListSyntax([]),
          name: .identifier("T"),
          colon: .colonToken(),
          inheritedType: SuppressedTypeSyntax(
            withoutTilde: .prefixOperator("~"),
            type: TypeSyntax(stringLiteral: "Copyable")
          )
        )
      ])
    )

    assertParse(
      """
      enum Whatever: Int, ~ Hashable, Equatable {}
      """,
      substructure: InheritanceClauseSyntax(
        colon: .colonToken(),
        inheritedTypes: InheritedTypeListSyntax([
          InheritedTypeSyntax(
            type: TypeSyntax(stringLiteral: "Int"),
            trailingComma: .commaToken()
          ),
          InheritedTypeSyntax(
            type: SuppressedTypeSyntax(
              withoutTilde: .prefixOperator("~"),
              type: TypeSyntax(stringLiteral: "Hashable")
            ),
            trailingComma: .commaToken()
          ),
          InheritedTypeSyntax(type: TypeSyntax(stringLiteral: "Equatable")),
        ])
      )
    )

    assertParse(
      """
      typealias T = 1️⃣~Int 2️⃣-> Bool
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "expected '(' to start function type",
          fixIts: ["insert '('"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected ')' in function type",
          fixIts: ["insert ')'"]
        ),
      ],
      fixedSource: """
        typealias T = (~Int) -> Bool
        """
    )

    assertParse(
      """
      typealias T = (~Int) -> Bool
      """,
      substructure: FunctionTypeSyntax(
        parameters: [
          TupleTypeElementSyntax(
            type: SuppressedTypeSyntax(
              withoutTilde: .prefixOperator("~"),
              type: IdentifierTypeSyntax(name: .identifier("Int"))
            )
          )
        ],
        returnClause: ReturnClauseSyntax(type: TypeSyntax("Bool"))
      )
    )
  }

  "testDoubleRightAngle" ignore AstFixture("") { cpg =>
    assertParse(
      "func foo<A>1️⃣> test()",
      diagnostics: [
        DiagnosticSpec(message: "unexpected code '> test' before parameter clause")
      ]
    )
  }

  "testUnexpectedTokenInClassFollowedByUnownedModifier" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class A ℹ️{
        1️⃣^
      }
      unowned 2️⃣B {
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code before modifier"),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "expected declaration and '}' after 'unowned' modifier",
          fixIts: ["insert declaration and '}'"]
        ),
      ],
      fixedSource:
        """
        class A {
          ^
        }
        unowned <#declaration#>
        }B {
        }
        """
    )
  }

  "testEmptyPrimaryAssociatedType" ignore AstFixture("") { cpg =>
    assertParse(
      "protocol X<1️⃣> {}",
      diagnostics: [
        DiagnosticSpec(message: "expected name in primary associated type clause", fixIts: ["insert name"])
      ],
      fixedSource: "protocol X<<#identifier#>> {}"
    )
  }

  "testCorrectIndentationWithNewline" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func
        test() {}1️⃣ var other: Int
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: ["insert newline", "insert ';'"]
        )
      ],
      fixedSource: """
        func
          test() {}
        var other: Int
        """
    )
  }

  "testMissingEqualInVariableDeclaration" ignore AstFixture("") { cpg =>
    assertParse(
      "let foo: [Int] 1️⃣[]",
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "expected '=' in variable", fixIts: ["insert '='"])
      ],
      fixedSource: "let foo: [Int] = []"
    )
  }

  "testInitAccessorsWithDefaultValues" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Test {
        var pair: (Int, Int) = (42, 0) {
          init(initialValue) {}

          get { (0, 42) }
          set { }
        }
      }
      """
    )

    assertParse(
      """
      struct Test {
        var pair: (Int, Int) = (42, 0) {
          get { (0, 42) }
          set { }

          init(initialValue1️⃣) {}
        }
      }
      """,
      substructure: InitializerDeclSyntax(
        initKeyword: .keyword(.`init`),
        signature: FunctionSignatureSyntax(
          parameterClause: FunctionParameterClauseSyntax(
            leftParen: .leftParenToken(),
            parameters: FunctionParameterListSyntax([
              FunctionParameterSyntax(
                firstName: .identifier("initialValue"),
                colon: .colonToken(presence: .missing),
                type: TypeSyntax(MissingTypeSyntax(placeholder: .identifier("<#type#>", presence: .missing)))
              )
            ]),
            rightParen: .rightParenToken(trailingTrivia: .space)
          )
        ),
        body: CodeBlockSyntax(
          leftBrace: .leftBraceToken(),
          statements: CodeBlockItemListSyntax([]),
          rightBrace: .rightBraceToken()
        )
      ),
      diagnostics: [
        DiagnosticSpec(message: "expected ':' and type in parameter", fixIts: ["insert ':' and type"])
      ],
      fixedSource:
        """
        struct Test {
          var pair: (Int, Int) = (42, 0) {
            get { (0, 42) }
            set { }

            init(initialValue: <#type#>) {}
          }
        }
        """
    )
  }

  "testBorrowingGetAccessor" ignore AstFixture("") { cpg =>
    assertParse(
      """
      struct Foo {
        var x: Int {
          borrowing get {}
        }
      }
      """
    )
  }

  "testDeclarationsWithPlaceholders" ignore AstFixture("") { cpg =>
    assertParse(
      """
      func 1️⃣<#name#>(2️⃣<#parameters#>3️⃣) -> 4️⃣<#return type#> {
          5️⃣<#function body#>
      }
      """,
      substructure: FunctionDeclSyntax(
        funcKeyword: .keyword(.func),
        name: .identifier("<#name#>"),
        signature: FunctionSignatureSyntax(
          parameterClause: FunctionParameterClauseSyntax(
            parameters: FunctionParameterListSyntax([
              FunctionParameterSyntax(
                firstName: .identifier("<#parameters#>"),
                colon: .colonToken(presence: .missing),
                type: MissingTypeSyntax(
                  placeholder: .identifier("<#type#>", presence: .missing)
                )
              )
            ])
          ),
          returnClause: ReturnClauseSyntax(
            type: IdentifierTypeSyntax(
              name: .identifier("<#return type#>")
            )
          )
        ),
        body: CodeBlockSyntax(
          statements: CodeBlockItemListSyntax([
            CodeBlockItemSyntax(
              item: .expr(
                ExprSyntax(
                  DeclReferenceExprSyntax(
                    baseName: .identifier("<#function body#>")
                  )
                )
              )
            )
          ])
        )
      ),
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "3️⃣", message: "expected ':' and type in parameter", fixIts: ["insert ':' and type"]),
        DiagnosticSpec(locationMarker: "4️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "5️⃣", message: "editor placeholder in source file"),
      ],
      fixedSource: """
        func <#name#>(<#parameters#>: <#type#>) -> <#return type#> {
            <#function body#>
        }
        """
    )

    assertParse(
      """
      "test1️⃣<#name#>" ignore AstFixture("") { cpg =>
          2️⃣<#function body#>
      }
      """,
      substructure: FunctionDeclSyntax(
        funcKeyword: .keyword(.func),
        name: .identifier("test"),
        UnexpectedNodesSyntax([
          TokenSyntax.identifier("<#name#>")
        ]),
        signature: FunctionSignatureSyntax(
          parameterClause: FunctionParameterClauseSyntax(
            parameters: FunctionParameterListSyntax([])
          )
        ),
        body: CodeBlockSyntax(
          statements: CodeBlockItemListSyntax([
            CodeBlockItemSyntax(
              item: .expr(
                ExprSyntax(
                  DeclReferenceExprSyntax(
                    baseName: .identifier("<#function body#>")
                  )
                )
              )
            )
          ])
        )
      ),
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "unexpected code '<#name#>' in function"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )

    assertParse(
      """
      class 1️⃣<#name#>: 2️⃣<#super class#> {
          3️⃣<#code#>
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "3️⃣", message: "editor placeholder in source file"),
      ]
    )

    assertParse(
      """
      enum 1️⃣<#name#> {
          case 2️⃣<#code#>
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )

    assertParse(
      """
      struct 1️⃣<#name#> {
          2️⃣<#code#>
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )

    assertParse(
      """
      protocol 1️⃣<#name#> {
          2️⃣<#code#>
      }
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )

    assertParse(
      """
      import 1️⃣<#name#>
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file")
      ]
    )

    assertParse(
      """
      typealias 1️⃣<#name#> = 2️⃣<#code#>
      """,
      diagnostics: [
        DiagnosticSpec(locationMarker: "1️⃣", message: "editor placeholder in source file"),
        DiagnosticSpec(locationMarker: "2️⃣", message: "editor placeholder in source file"),
      ]
    )
  }

  // https://github.com/apple/swift-syntax/issues/2273
  "testEnumCaseWithGenericParameter" ignore AstFixture("") { cpg =>
    assertParse(
      """
      enum Foo {
        case foo1️⃣<T>(T)
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case bar1️⃣<T>(param: T)
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case baz1️⃣<T>
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case one, two1️⃣<T>
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case three1️⃣<T>, four
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case five1️⃣<T>(param: T), six
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        )
      ]
    )

    assertParse(
      """
      enum Foo {
        case seven1️⃣<T>, eight2️⃣<U>
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "generic signature cannot be declared in enum 'case'"
        ),
      ]
    )

    assertParse(
      """
      enum Foo<T> {
        case five(param: T), six
      }
      """
    )
  }

  "testLiteralInitializerWithTrailingClosure" ignore AstFixture("") { cpg =>
    assertParse(
      "let foo = 1 { return 1 }",
      substructure: AccessorBlockSyntax(accessors: .getter([CodeBlockItemSyntax("return 1")]))
    )
  }

  "testResultDependsOnSelf" ignore AstFixture("") { cpg =>
    assertParse(
      """
      class MethodModifiers {
         _resultDependsOnSelf func getDependentResult() -> Builtin.NativeObject {
           return Builtin.unsafeCastToNativeObject(self)
         }
       }
      """,
      experimentalFeatures: .nonEscapableTypes
    )

    assertParse(
      """
        class MethodModifiers {
           _resultDependsOnSelf func _resultDependsOnSelf() -> Builtin.NativeObject {
             return Builtin.unsafeCastToNativeObject(self)
           }
         }
      """,
      experimentalFeatures: .nonEscapableTypes
    )
  }
}
