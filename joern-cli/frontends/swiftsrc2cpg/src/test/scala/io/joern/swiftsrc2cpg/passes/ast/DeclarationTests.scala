package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class DeclarationTests extends AstSwiftSrc2CpgSuite {

  "DeclarationTests" should {

    "testImports" ignore {
      val cpg = code("""
      |import Foundation
      |@_spi(Private) import SwiftUI
      |@_exported import class Foundation.Thread
      |@_private(sourceFile: "YetAnotherFile.swift") import Foundation
      |""".stripMargin)
      ???
    }

    "testStructParsing" ignore {
      val cpg = code("struct Foo {}")
      ???
    }

    "testFuncParsing" ignore {
      val cpg = code("""
      |func foo1() {}
      |func foo2() -> Slice<MinimalMutableCollection<T>> {}
      |func onEscapingAutoclosure(_ fn: @Sendable @autoclosure @escaping () -> Int) {}
      |func onEscapingAutoclosure2(_ fn: @escaping @autoclosure @Sendable () -> Int) {}
      |func bar(_ : String) async -> [[String]: Array<String>] {}
      |func tupleMembersFunc() -> (Type.Inner, Type2.Inner2) {}
      |func myFun<S: T & U>(var1: S) {
      |  // do stuff
      |}
      |func /^/ (lhs: Int, rhs: Int) -> Int { 1 / 2 }
      |func /^ (lhs: Int, rhs: Int) -> Int { 1 / 2 }
      |func name(_ default: Int) {}
      |class MyClass {
      |  func foo1()
      |  func foo2<Int>()
      |}
      |""".stripMargin)
      ???
    }

    "testClassParsing" ignore {
      val cpg = code("""
      |class Foo {}
      |@dynamicMemberLookup @available(swift 4.0)
      |public class MyClass {
      |  let A: Int
      |  let B: Double
      |}
      |struct A<@NSApplicationMain T: AnyObject> {}
      |class T where t {}
      |class B<where g> {}
      |""".stripMargin)
      ???
    }

    "testActorParsing" ignore {
      val cpg = code("""
      |actor Foo1 {}
      |actor Foo2 {
      |  nonisolated init?() {
      |    for (x, y, z) in self.triples {
      |      precondition(isSafe)
      |    }
      |  }
      |  subscript(_ param: String) -> Int {
      |    return 42
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testNonisolatedUnsafeParsing" ignore {
      val cpg = code("""
      |nonisolated(unsafe) let a = 0
      |struct A {
      |  nonisolated(unsafe) let b = 0
      |  nonisolated(unsafe) var c: Int { 0 }
      |  nonisolated(unsafe) let d = 0
      |}
      |""".stripMargin)
      ???
    }

    "testProtocolParsing" ignore {
      val cpg = code("""
      |protocol Foo {}
      |protocol P { init() }
      |protocol P {
      |  associatedtype Foo: Bar where X.Y == Z.W.W.Self
      |
      |  var foo: Bool { get set }
      |  subscript<R>(index: Int) -> R
      |}
      |""".stripMargin)
      ???
    }

    "testVariableDeclarations" ignore {
      val cpg = code("""
      |z
      |var x: Double = z
      |async let a = fetch("1.jpg")
      |async let b: Image = fetch("2.jpg")
      |private unowned(unsafe) var foo: Int
      |unowned(unsafe) let unmanagedVar: Class = c
      |_ = foo?.description
      |@Wrapper var café = 42
      |var y: T {
      |  get async {
      |    foo()
      |     bar()
      |  }
      |}
      |var foo1: Int {
      |  _read {
      |    yield 1234567890
      |  }
      |  _modify {
      |    var someLongVariable = 0
      |     yield &someLongVariable
      |  }
      |}
      |var foo2: Int {
      |  @available(swift 5.0)
      |  func myFun() -> Int {
      |    return 42
      |  }
      |  return myFun()
      |}
      |var foo3: Int {
      |  mutating set {
      |    test += 1
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testTypealias" ignore {
      val cpg = code("""
      |typealias Foo = Int
      |typealias MyAlias = (_ a: Int, _ b: Double, _ c: Bool, _ d: String) -> Bool
      |typealias A = @attr1 @attr2(hello) (Int) -> Void
      |""".stripMargin)
      ???
    }

    "testPrecedenceGroup" ignore {
      val cpg = code("""
      |precedencegroup FooGroup {
      |  higherThan: Group1, Group2
      |  lowerThan: Group3, Group4
      |  associativity: left
      |  assignment: false
      |}
      |precedencegroup FunnyPrecedence {
      |  associativity: left
      |  higherThan: MultiplicationPrecedence
      |}
      |""".stripMargin)
      ???
    }

    "testOperators" ignore {
      val cpg = code("""
      |infix operator *-* : FunnyPrecedence
      |infix operator  <*<<< : MediumPrecedence, &
      |prefix operator ^^ : PrefixMagicOperatorProtocol
      |infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
      |postfix operator ^^ : PostfixMagicOperatorProtocol
      |infix operator ^^ : PostfixMagicOperatorProtocol, Class, Struct
      |""".stripMargin)
      ???
    }

    "testObjCAttribute" ignore {
      val cpg = code("""
      |@objc(
      |  thisMethodHasAVeryLongName:
      |  foo:
      |  bar:
      |)
      |func f() {}
      |""".stripMargin)
      ???
    }

    "testParsePoundError" ignore {
      val cpg = code("""#error("Unsupported platform")""")
      ???
    }

    "testParsePoundWarning" ignore {
      val cpg = code("""#warning("Unsupported platform")""")
      ???
    }

    "testParseRetroactiveExtension" ignore {
      val cpg = code("extension Int: @retroactive Identifiable {}")
      ???
    }

    "testEnumParsing" ignore {
      val cpg = code("""
      |enum Foo {
      |  @preconcurrency case custom(@Sendable () throws -> Void)
      |}
      |enum Content {
      |  case keyPath(KeyPath<FocusedValues, Value?>)
      |  case keyPath(KeyPath<FocusedValues, Binding<Value>?>)
      |  case value(Value?)
      |}
      |""".stripMargin)
      ???
    }

    "testMissingColonInFunctionSignature" ignore {
      val cpg = code("func test(first second: Int)")
      ???
    }

    "testNoParamsForFunction" ignore {
      val cpg = code("""
      |class MyClass {
      |  func withoutParameters()
      |  func withParameters() {}
      |}
      |""".stripMargin)
      ???
    }

    "testAccessors" ignore {
      val cpg = code("""
      |var foo1 : Int {
      |  _read async { 0 }
      |}
      |var foo2 : Int {
      |  get async { 0 }
      |}
      |""".stripMargin)
      ???
    }

    "testInitAccessor" ignore {
      val cpg = code("""
      |struct S1 {
      |  var value: Int {
      |    init {}
      |    get {}
      |    set {}
      |  }
      |}
      |struct S2 {
      |  let _value: Int
      |  init() {}
      |}
      |struct S3 {
      |  var value: Int {
      |    init(newValue) {}
      |    get {}
      |    set(newValue) {}
      |  }
      |}
      |struct S4 {
      |  var value: Int {
      |    init(newValue) {}
      |    get {}
      |    set(newValue) {}
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testInitializers" ignore {
      val cpg = code("""
      |struct S {
      |  init!(int: Int) { }
      |  init! (uint: UInt) { }
      |  init !(float: Float) { }
      |
      |  init?(string: String) { }
      |  init ?(double: Double) { }
      |  init ? (char: Character) { }
      |}
      |""".stripMargin)
      ???
    }

    "testDeinitializers" ignore {
      val cpg = code("""
      |struct S {
      |  deinit {}
      |  deinit
      |}
      |""".stripMargin)
      ???
    }

    "testAttributedMember" ignore {
      val cpg = code("""
      |struct Foo {
      |  @Argument(help: "xxx")
      |  var generatedPath: String
      |}
      |""".stripMargin)
      ???
    }

    "testAnyAsParameterLabel" ignore {
      val cpg = code("func at(any kinds: [RawTokenKind]) {}")
      ???
    }

    "testPublicClass" ignore {
      val cpg = code("public class Foo: Superclass {}")
      ???
    }

    "testReasyncFunctions" ignore {
      val cpg = code("""
      |class MyType {
      |  init(_ f: () async -> Void) reasync {
      |    await f()
      |  }
      |  func foo(index: Int) reasync rethrows -> String {
      |    await f()
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testNestedStructs" ignore {
      val cpg = code("""
      |struct Foo {
      |  struct Bar {}
      |}
      |""".stripMargin)
      ???
    }

    "testMacroExpansionDeclarationWithKeywordName" ignore {
      val cpg = code("""
      |struct X {
      |  #case
      |}""".stripMargin)
      ???
    }

    "testVariableFollowedByReferenceToSet" ignore {
      val cpg = code("""
      |func bar() {
      |  let a = b
      |  set.c
      |}
      |""".stripMargin)
      ???
    }

    "testMacroDecl" ignore {
      val cpg = code("""
      |macro m1(): Int = A.M1
      |macro m2(_: Int) = A.M2
      |macro m3(a b: Int) -> Int = A.M3
      |macro m4<T>(): T = A.M4 where T.Assoc: P
      |macro m5<T: P>(_: T)
      |""".stripMargin)
      ???
    }

    "testClassWithPrivateSet" ignore {
      val cpg = code("""
      |struct Properties {
      |  class private(set) var privateSetterCustomNames: Bool
      |}
      |""".stripMargin)
      ???
    }

    "testOpenVarInCodeBlockItemList" ignore {
      val cpg = code("""
      |func test() {
      |  open var foo = 2
      |}
      |""".stripMargin)
      ???
    }

    "testAsyncLetInLocalContext" ignore {
      val cpg = code("""
      |func foo() async {
      |  async let x: String = "x"
      |}
      |""".stripMargin)
      ???
    }

    "testBorrowingConsumingParameterSpecifiers1" ignore {
      val cpg = code("""
      |struct borrowing {}
      |struct consuming {}
      |struct Foo {}
      |func foo(x: borrowing Foo) {}
      |func bar(x: consuming Foo) {}
      |func baz(x: (borrowing Foo, consuming Foo) -> ()) {}
      |""".stripMargin)
      ???
    }

    "testBorrowingConsumingParameterSpecifiers2" ignore {
      val cpg = code(
        // `borrowing` and `consuming` are contextual keywords, so they should also
        // continue working as type and/or parameter names
        """
      |func zim(x: borrowing) {}
      |func zang(x: consuming) {}
      |func zung(x: borrowing consuming) {}
      |func zip(x: consuming borrowing) {}
      |func zap(x: (borrowing, consuming) -> ()) {}
      |func zoop(x: (borrowing consuming, consuming borrowing) -> ()) {}
      |""".stripMargin
      )
      ???
    }

    "testSuppressedImplicitConformance" ignore {
      val cpg = code("""
      |struct Hello: ~Copyable {}
      |let foo: any ~Copyable = 0
      |typealias X = ~Copyable.Type
      |typealias Y = ~A.B.C
      |typealias Z1 = ~A?
      |typealias Z2 = ~A<T>
      |struct Hello<T: ~Copyable> {}
      |func bar<T: ~Copyable>(_ t: T) {}
      |enum Whatever: Int, ~ Hashable, Equatable {}
      |""".stripMargin)
      ???
    }

    "testArrayDeclaration" in {
      val cpg               = code("let foo: [Int] = []")
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(assignment) = methodBlock.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(arrayCall) = assignment.astChildren.isCall.l
      arrayCall.name shouldBe Operators.arrayInitializer
      arrayCall.code shouldBe "[]"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(fooIdent) = assignment.astChildren.isIdentifier.l
      fooIdent.name shouldBe "foo"
      fooIdent.typeFullName shouldBe "[Int]"
    }

    "testSetDeclaration" in {
      val cpg               = code("var foo: Set<Int> = [1, 2, 3]")
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(assignment) = methodBlock.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(arrayCall) = assignment.astChildren.isCall.l
      arrayCall.name shouldBe Operators.arrayInitializer
      arrayCall.code shouldBe "[1, 2, 3]"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayCall.argument.isLiteral.code.l shouldBe List("1", "2", "3")

      val List(fooIdent) = assignment.astChildren.isIdentifier.l
      fooIdent.name shouldBe "foo"
      fooIdent.typeFullName shouldBe "Set<Int>"
    }

    "testDictionaryDeclaration" in {
      val cpg               = code("var numbers = [1: \"One\", 2: \"Two\", 3: \"Three\"]")
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(assignment) = methodBlock.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(arrayCall) = assignment.astChildren.isCall.l
      arrayCall.name shouldBe Operators.arrayInitializer
      arrayCall.code shouldBe "[1: \"One\", 2: \"Two\", 3: \"Three\"]"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayCall.argument.isCall.code.l shouldBe List("1: \"One\",", "2: \"Two\",", "3: \"Three\"")
    }

    "testAddDictionaryElements" in {
      val cpg = code("""
        |var elements = ["A": "1", "B": "2"]
        |elements["A"] = "3"
        |print(elements["A"])
        |""".stripMargin)
      val List(elementsAccess1, elementsAccess2) = cpg.call(Operators.indexAccess).l
      elementsAccess1.code shouldBe "elements[\"A\"]"
      val List(arg11) = elementsAccess1.argument(1).start.isIdentifier.l
      arg11.name shouldBe "elements"
      val List(arg12) = elementsAccess1.argument(2).start.isLiteral.l
      arg12.code shouldBe "\"A\""

      elementsAccess2.code shouldBe "elements[\"A\"]"
      val List(arg21) = elementsAccess2.argument(1).start.isIdentifier.l
      arg21.name shouldBe "elements"
      val List(arg22) = elementsAccess2.argument(2).start.isLiteral.l
      arg22.code shouldBe "\"A\""
    }

    "testTupleDeclaration" in {
      val cpg               = code("var product = (\"MacBook\", 1099.99)")
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l

      val List(assignment) = methodBlock.astChildren.isCall.l
      assignment.name shouldBe Operators.assignment

      val List(arrayCall) = assignment.astChildren.isCall.l
      arrayCall.name shouldBe Operators.arrayInitializer
      arrayCall.code shouldBe "(\"MacBook\", 1099.99)"
      arrayCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayCall.argument.isLiteral.code.l shouldBe List("\"MacBook\"", "1099.99")
    }

    "testTupleAccess" in {
      val cpg = code("""
        |var product = ("MacBook", 1099.99)
        |print("Name:", product.0)
        |print("Price:", product.1)
        |""".stripMargin)
      val List(elementsAccess1, elementsAccess2) = cpg.call(Operators.indexAccess).l
      elementsAccess1.code shouldBe "product[0]"
      val List(arg11) = elementsAccess1.argument(1).start.isIdentifier.l
      arg11.name shouldBe "product"
      val List(arg12) = elementsAccess1.argument(2).start.isLiteral.l
      arg12.code shouldBe "0"

      elementsAccess2.code shouldBe "product[1]"
      val List(arg21) = elementsAccess2.argument(1).start.isIdentifier.l
      arg21.name shouldBe "product"
      val List(arg22) = elementsAccess2.argument(2).start.isLiteral.l
      arg22.code shouldBe "1"
    }

    "testInitAccessorsWithDefaultValues" ignore {
      val cpg = code("""
      |struct Test {
      |  var pair: (Int, Int) = (42, 0) {
      |    init(initialValue) {}
      |    get { (0, 42) }
      |     set { }
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testBorrowingGetAccessor" ignore {
      val cpg = code("""
      |struct Foo {
      |  var x: Int {
      |    borrowing get {}
      |  }
      |}
      |""".stripMargin)
      ???
    }

    "testLiteralInitializerWithTrailingClosure" ignore {
      val cpg = code("let foo = 1 { return 1 }")
      ???
    }

  }
}
