package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class DeclarationTests extends AbstractPassTest {

  "DeclarationTests" should {

    "testImports" ignore AstFixture("""
      |import Foundation
      |@_spi(Private) import SwiftUI
      |@_exported import class Foundation.Thread
      |@_private(sourceFile: "YetAnotherFile.swift") import Foundation
      |""".stripMargin) { cpg => ??? }

    "testStructParsing" ignore AstFixture("struct Foo {}") { cpg => ??? }

    "testFuncParsing" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testClassParsing" ignore AstFixture("""
      |class Foo {}
      |@dynamicMemberLookup @available(swift 4.0)
      |public class MyClass {
      |  let A: Int
      |  let B: Double
      |}
      |struct A<@NSApplicationMain T: AnyObject> {}
      |class T where t {}
      |class B<where g> {}
      |""".stripMargin) { cpg => ??? }

    "testActorParsing" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testNonisolatedUnsafeParsing" ignore AstFixture("""
      |nonisolated(unsafe) let a = 0
      |struct A {
      |  nonisolated(unsafe) let b = 0
      |  nonisolated(unsafe) var c: Int { 0 }
      |  nonisolated(unsafe) let d = 0
      |}
      |""".stripMargin) { cpg => ??? }

    "testProtocolParsing" ignore AstFixture("""
      |protocol Foo {}
      |protocol P { init() }
      |protocol P {
      |  associatedtype Foo: Bar where X.Y == Z.W.W.Self
      |
      |  var foo: Bool { get set }
      |  subscript<R>(index: Int) -> R
      |}
      |""".stripMargin) { cpg => ??? }

    "testVariableDeclarations" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testTypealias" ignore AstFixture("""
      |typealias Foo = Int
      |typealias MyAlias = (_ a: Int, _ b: Double, _ c: Bool, _ d: String) -> Bool
      |typealias A = @attr1 @attr2(hello) (Int) -> Void
      |""".stripMargin) { cpg => ??? }

    "testPrecedenceGroup" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testOperators" ignore AstFixture("""
      |infix operator *-* : FunnyPrecedence
      |infix operator  <*<<< : MediumPrecedence, &
      |prefix operator ^^ : PrefixMagicOperatorProtocol
      |infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
      |postfix operator ^^ : PostfixMagicOperatorProtocol
      |infix operator ^^ : PostfixMagicOperatorProtocol, Class, Struct
      |""".stripMargin) { cpg => ??? }

    "testObjCAttribute" ignore AstFixture("""
      |@objc(
      |  thisMethodHasAVeryLongName:
      |  foo:
      |  bar:
      |)
      |func f() {}
      |""".stripMargin) { cpg => ??? }

    "testParsePoundError" ignore AstFixture("""#error("Unsupported platform")""") { cpg => ??? }

    "testParsePoundWarning" ignore AstFixture("""#warning("Unsupported platform")""") { cpg => ??? }

    "testParseRetroactiveExtension" ignore AstFixture("extension Int: @retroactive Identifiable {}") { cpg => ??? }

    "testEnumParsing" ignore AstFixture("""
      |enum Foo {
      |  @preconcurrency case custom(@Sendable () throws -> Void)
      |}
      |enum Content {
      |  case keyPath(KeyPath<FocusedValues, Value?>)
      |  case keyPath(KeyPath<FocusedValues, Binding<Value>?>)
      |  case value(Value?)
      |}
      |""".stripMargin) { cpg => ??? }

    "testMissingColonInFunctionSignature" ignore AstFixture("func test(first second: Int)") { cpg => ??? }

    "testNoParamsForFunction" ignore AstFixture("""
      |class MyClass {
      |  func withoutParameters()
      |  func withParameters() {}
      |}
      |""".stripMargin) { cpg => ??? }

    "testAccessors" ignore AstFixture("""
      |var foo1 : Int {
      |  _read async { 0 }
      |}
      |var foo2 : Int {
      |  get async { 0 }
      |}
      |""".stripMargin) { cpg => ??? }

    "testInitAccessor" ignore AstFixture("""
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
      |""".stripMargin) { cpg => ??? }

    "testInitializers" ignore AstFixture("""
      |struct S {
      |  init!(int: Int) { }
      |  init! (uint: UInt) { }
      |  init !(float: Float) { }
      |
      |  init?(string: String) { }
      |  init ?(double: Double) { }
      |  init ? (char: Character) { }
      |}
      |""".stripMargin) { cpg => ??? }

    "testDeinitializers" ignore AstFixture("""
      |struct S {
      |  deinit {}
      |  deinit
      |}
      |""".stripMargin) { cpg => ??? }

    "testAttributedMember" ignore AstFixture("""
      |struct Foo {
      |  @Argument(help: "xxx")
      |  var generatedPath: String
      |}
      |""".stripMargin) { cpg => ??? }

    "testAnyAsParameterLabel" ignore AstFixture("func at(any kinds: [RawTokenKind]) {}") { cpg => ??? }

    "testPublicClass" ignore AstFixture("public class Foo: Superclass {}") { cpg => ??? }

    "testReasyncFunctions" ignore AstFixture("""
      |class MyType {
      |  init(_ f: () async -> Void) reasync {
      |    await f()
      |  }
      |  func foo(index: Int) reasync rethrows -> String {
      |    await f()
      |  }
      |}
      |""".stripMargin) { cpg => ??? }

    "testNestedStructs" ignore AstFixture("""
      |struct Foo {
      |  struct Bar {}
      |}
      |""".stripMargin) { cpg => ??? }

    "testMacroExpansionDeclarationWithKeywordName" ignore AstFixture("""
      |struct X {
      |  #case
      |}""".stripMargin) { cpg => ??? }

    "testVariableFollowedByReferenceToSet" ignore AstFixture("""
      |func bar() {
      |  let a = b
      |  set.c
      |}
      |""".stripMargin) { cpg => ??? }

    "testMacroDecl" ignore AstFixture("""
      |macro m1(): Int = A.M1
      |macro m2(_: Int) = A.M2
      |macro m3(a b: Int) -> Int = A.M3
      |macro m4<T>(): T = A.M4 where T.Assoc: P
      |macro m5<T: P>(_: T)
      |""".stripMargin) { cpg => ??? }

    "testClassWithPrivateSet" ignore AstFixture("""
      |struct Properties {
      |  class private(set) var privateSetterCustomNames: Bool
      |}
      |""".stripMargin) { cpg => ??? }

    "testOpenVarInCodeBlockItemList" ignore AstFixture("""
      |func test() {
      |  open var foo = 2
      |}
      |""".stripMargin) { cpg => ??? }

    "testAsyncLetInLocalContext" ignore AstFixture("""
      |func foo() async {
      |  async let x: String = "x"
      |}
      |""".stripMargin) { cpg => ??? }

    "testBorrowingConsumingParameterSpecifiers1" ignore AstFixture("""
      |struct borrowing {}
      |struct consuming {}
      |struct Foo {}
      |func foo(x: borrowing Foo) {}
      |func bar(x: consuming Foo) {}
      |func baz(x: (borrowing Foo, consuming Foo) -> ()) {}
      |""".stripMargin) { cpg => ??? }

    "testBorrowingConsumingParameterSpecifiers2" ignore AstFixture(
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
    ) { cpg => ??? }

    "testSuppressedImplicitConformance" ignore AstFixture("""
      |struct Hello: ~Copyable {}
      |let foo: any ~Copyable = 0
      |typealias X = ~Copyable.Type
      |typealias Y = ~A.B.C
      |typealias Z1 = ~A?
      |typealias Z2 = ~A<T>
      |struct Hello<T: ~Copyable> {}
      |func bar<T: ~Copyable>(_ t: T) {}
      |enum Whatever: Int, ~ Hashable, Equatable {}
      |""".stripMargin) { cpg => ??? }

    "testArrayDeclaration" in AstFixture("let foo: [Int] = []") { cpg =>
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

    "testSetDeclaration" in AstFixture("var foo: Set<Int> = [1, 2, 3]") { cpg =>
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

    "testDictionaryDeclaration" in AstFixture("var numbers = [1: \"One\", 2: \"Two\", 3: \"Three\"]") { cpg =>
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

    "testAddDictionaryElements" in AstFixture("""
        |var elements = ["A": "1", "B": "2"]
        |elements["A"] = "3"
        |print(elements["A"])
        |""".stripMargin) { cpg =>
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

    "testTupleDeclaration" in AstFixture("var product = (\"MacBook\", 1099.99)") { cpg =>
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

    "testTupleAccess" in AstFixture("""
        |var product = ("MacBook", 1099.99)
        |print("Name:", product.0)
        |print("Price:", product.1)
        |""".stripMargin) { cpg =>
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

    "testInitAccessorsWithDefaultValues" ignore AstFixture("""
      |struct Test {
      |  var pair: (Int, Int) = (42, 0) {
      |    init(initialValue) {}
      |    get { (0, 42) }
      |     set { }
      |  }
      |}
      |""".stripMargin) { cpg => ??? }

    "testBorrowingGetAccessor" ignore AstFixture("""
      |struct Foo {
      |  var x: Int {
      |    borrowing get {}
      |  }
      |}
      |""".stripMargin) { cpg => ??? }

    "testLiteralInitializerWithTrailingClosure" ignore AstFixture("let foo = 1 { return 1 }") { cpg => ??? }

  }
}
