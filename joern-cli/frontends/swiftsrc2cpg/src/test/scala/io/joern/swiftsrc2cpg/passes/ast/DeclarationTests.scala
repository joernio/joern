package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class DeclarationTests extends SwiftSrc2CpgSuite {

  "DeclarationTests" should {

    "testImports" in {
      val cpg = code("""
      |import Foundation
      |@_spi(Private) import SwiftUI
      |@_exported import class Foundation.Thread
      |@_private(sourceFile: "YetAnotherFile.swift") import Foundation
      |""".stripMargin)
      cpg.imports.importedEntity.l should contain allOf ("Foundation", "SwiftUI")
    }

    "testStructParsing" in {
      val cpg              = code("struct Foo {}")
      val List(structDecl) = cpg.typeDecl.nameExact("Foo").l
      structDecl.fullName shouldBe "Test0.swift:<global>.Foo"
    }

    "testFuncParsing" in {
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
      cpg.method.filename("Test0.swift").nameNot("<global>").name.l should contain allOf (
        "foo1",
        "foo2",
        "onEscapingAutoclosure",
        "onEscapingAutoclosure2",
        "bar",
        "tupleMembersFunc",
        "myFun",
        "/^/",
        "/^",
        "name"
      )
      val List(myClassDecl) = cpg.typeDecl.nameExact("MyClass").l
      myClassDecl.method.name.l should contain allOf ("foo1", "foo2")
    }

    "testClassParsing" in {
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
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf (
        "Foo",
        "MyClass",
        "A",
        "T",
        "B"
      )
      val List(myClass) = cpg.typeDecl.nameExact("MyClass").l
      myClass.member.name.l shouldBe List("A", "B")
      myClass.member.typeFullName.l shouldBe List("Swift.Int", "Swift.Double")
    }

    "testActorParsing" in {
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
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf ("Foo1", "Foo2")
      val List(foo2) = cpg.typeDecl.nameExact("Foo2").l
      foo2.method.name.l should contain("init")
    }

    "testNonisolatedUnsafeParsing" in {
      val cpg = code("""
      |nonisolated(unsafe) let a = 0
      |struct A {
      |  nonisolated(unsafe) let b = 0
      |  nonisolated(unsafe) var c: Int { 0 }
      |  nonisolated(unsafe) let d = 0
      |}
      |""".stripMargin)
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      globalBlock.local.nameExact("a").l should not be empty
      val List(structA) = cpg.typeDecl.nameExact("A").l
      structA.member.name.toSet shouldBe Set("b", "c", "d")
    }

    "testProtocolParsing" in {
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
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf ("Foo", "P")
      val ps = cpg.typeDecl.filename("Test0.swift").nameExact("P").l
      ps.name.l shouldBe List("P", "P")
      ps.fullName.toSet shouldBe Set("Test0.swift:<global>.P", "Test0.swift:<global>.P<duplicate>0")
      ps.flatMap(_.method.name.l) should contain("init")
    }

    "testVariableDeclarations" in {
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
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      globalBlock.local.name.l should contain allOf ("x", "a", "b", "foo", "unmanagedVar", "café")
      val List(localX) = globalBlock.local.nameExact("x").l
      localX.typeFullName shouldBe "Swift.Double"
      val List(cafeLocal) = globalBlock.local.nameExact("café").l
      cafeLocal.code shouldBe "café"
      cpg.method.filename("Test0.swift").name.l should contain allOf (
        "y.getter",
        "foo1._read",
        "foo1._modify",
        "foo3.setter"
      )
    }

    "testTypealias" in {
      val cpg = code("""
      |typealias Foo = Int
      |typealias MyAlias = (_ a: Int, _ b: Double, _ c: Bool, _ d: String) -> Bool
      |typealias A = @attr1 @attr2(hello) (Int) -> Void
      |""".stripMargin)
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf ("Foo", "MyAlias", "A")
      val List(fooAlias) = cpg.typeDecl.nameExact("Foo").l
      fooAlias.aliasTypeFullName.headOption shouldBe Some("Swift.Int")
    }

    "testPrecedenceGroup" in {
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
      // precedencegroup declarations are parser-only — the global method body is empty.
      val List(globalMethod) = cpg.method.nameExact("<global>").filename("Test0.swift").l
      globalMethod.block.astChildren.l shouldBe empty
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").l shouldBe empty
    }

    "testOperators" in {
      val cpg = code("""
      |infix operator *-* : FunnyPrecedence
      |infix operator  <*<<< : MediumPrecedence, &
      |prefix operator ^^ : PrefixMagicOperatorProtocol
      |infix operator  <*< : MediumPrecedence, InfixMagicOperatorProtocol
      |postfix operator ^^ : PostfixMagicOperatorProtocol
      |infix operator ^^ : PostfixMagicOperatorProtocol, Class, Struct
      |""".stripMargin)
      // Top-level operator declarations are parser-only — the global method body is empty.
      val List(globalMethod) = cpg.method.nameExact("<global>").filename("Test0.swift").l
      globalMethod.block.astChildren.l shouldBe empty
      cpg.method.filename("Test0.swift").nameNot("<global>").l shouldBe empty
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").l shouldBe empty
    }

    "testObjCAttribute" in {
      val cpg = code("""
      |@objc(
      |  thisMethodHasAVeryLongName:
      |  foo:
      |  bar:
      |)
      |func f() {}
      |""".stripMargin)
      val List(fMethod) = cpg.method.filename("Test0.swift").nameExact("f").l
      fMethod.fullName should startWith("Test0.swift:<global>.f")
    }

    "testParsePoundError" in {
      val cpg = code("""#error("Unsupported platform")""")
      // #error is parsed as a macro expansion call.
      val List(errorCall) = cpg.call.nameExact("error").l
      errorCall.code shouldBe """#error("Unsupported platform")"""
      errorCall.methodFullName shouldBe "error"
      val List(arg) = errorCall.argument.isLiteral.l
      arg.code shouldBe "\"Unsupported platform\""
      arg.typeFullName shouldBe Defines.String
    }

    "testParsePoundWarning" in {
      val cpg = code("""#warning("Unsupported platform")""")
      // #warning is parsed as a macro expansion call.
      val List(warnCall) = cpg.call.nameExact("warning").l
      warnCall.code shouldBe """#warning("Unsupported platform")"""
      warnCall.methodFullName shouldBe "warning"
      val List(arg) = warnCall.argument.isLiteral.l
      arg.code shouldBe "\"Unsupported platform\""
      arg.typeFullName shouldBe Defines.String
    }

    "testParseRetroactiveExtension" in {
      val cpg = code("extension Int: @retroactive Identifiable {}")
      // The extension is represented as a TypeRef pointing at the existing builtin `Swift.Int` TypeDecl.
      val List(extRef) = cpg.typeRef.l
      extRef.code shouldBe "extension Int: @retroactive Identifiable {}"
      extRef.typeFullName shouldBe "Test0.swift:<global>.Swift.Int"
      // The retroactive inherit target `Identifiable` is registered as a TypeDecl in the CPG.
      val List(identifiableDecl) = cpg.typeDecl.nameExact("Identifiable").l
      identifiableDecl.fullName shouldBe "Identifiable"
    }

    "testEnumParsing" in {
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
      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.member.name.l shouldBe List("custom")
      val List(content) = cpg.typeDecl.nameExact("Content").l
      content.member.name.l shouldBe List("keyPath", "keyPath", "value")
    }

    "testMissingColonInFunctionSignature" in {
      val cpg              = code("func test(first second: Int)")
      val List(testMethod) = cpg.method.nameExact("test").filename("Test0.swift").l
      testMethod.parameter.name.l should contain("second")
    }

    "testNoParamsForFunction" in {
      val cpg = code("""
      |class MyClass {
      |  func withoutParameters()
      |  func withParameters() {}
      |}
      |""".stripMargin)
      val List(myClass) = cpg.typeDecl.nameExact("MyClass").l
      myClass.method.name.l should contain allOf ("withoutParameters", "withParameters")
    }

    "testAccessors" in {
      val cpg = code("""
      |var foo1 : Int {
      |  _read async { 0 }
      |}
      |var foo2 : Int {
      |  get async { 0 }
      |}
      |""".stripMargin)
      cpg.method.filename("Test0.swift").name.l should contain allOf ("foo1._read", "foo2.getter")
    }

    "testInitAccessor" in {
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
      val List(s1) = cpg.typeDecl.nameExact("S1").l
      s1.method.name.l should contain allOf ("value.getter", "value.setter")
      val List(s2) = cpg.typeDecl.nameExact("S2").l
      s2.member.name.l shouldBe List("_value")
      s2.method.name.l should contain("init")
    }

    "testInitializers" in {
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
      val List(s)          = cpg.typeDecl.nameExact("S").l
      val List(initMethod) = s.method.nameExact("init").l
      initMethod.fullName shouldBe "Test0.swift:<global>.S.init:(int:Swift.Int)->Test0.swift:<global>.S"
      initMethod.signature shouldBe "(int:Swift.Int)->Test0.swift:<global>.S"
      initMethod.code shouldBe "init!(int: Int) { }"
      initMethod.parameter.name.l shouldBe List("self", "int")
      initMethod.parameter.typeFullName.l shouldBe List("Test0.swift:<global>.S", "Swift.Int")
    }

    "testDeinitializers" in {
      val cpg = code("""
      |struct S {
      |  deinit {}
      |  deinit
      |}
      |""".stripMargin)
      val List(s) = cpg.typeDecl.nameExact("S").l
      s.method.name.l should contain("deinit")
    }

    "testAttributedMember" in {
      val cpg = code("""
      |struct Foo {
      |  @Argument(help: "xxx")
      |  var generatedPath: String
      |}
      |""".stripMargin)
      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.member.name.l shouldBe List("generatedPath")
      foo.member.typeFullName.l shouldBe List("Swift.String")
    }

    "testAnyAsParameterLabel" in {
      val cpg            = code("func at(any kinds: [RawTokenKind]) {}")
      val List(atMethod) = cpg.method.nameExact("at").filename("Test0.swift").l
      atMethod.parameter.name.l should contain("kinds")
    }

    "testPublicClass" in {
      val cpg          = code("public class Foo: Superclass {}")
      val List(fooDec) = cpg.typeDecl.nameExact("Foo").l
      fooDec.inheritsFromTypeFullName.l should contain("Superclass")
    }

    "testReasyncFunctions" in {
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
      val List(myType) = cpg.typeDecl.nameExact("MyType").l
      myType.method.name.l should contain allOf ("init", "foo")
    }

    "testNestedStructs" in {
      val cpg = code("""
      |struct Foo {
      |  struct Bar {}
      |}
      |""".stripMargin)
      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.fullName shouldBe "Test0.swift:<global>.Foo"
      val List(bar) = cpg.typeDecl.nameExact("Bar").l
      bar.fullName shouldBe "Test0.swift:<global>.Foo.Bar"
    }

    "testMacroExpansionDeclarationWithKeywordName" in {
      val cpg = code("""
      |struct X {
      |  #case
      |}""".stripMargin)
      val List(x) = cpg.typeDecl.nameExact("X").l
      x.fullName shouldBe "Test0.swift:<global>.X"
    }

    "testVariableFollowedByReferenceToSet" in {
      val cpg = code("""
      |func bar() {
      |  let a = b
      |  set.c
      |}
      |""".stripMargin)
      val List(barMethod) = cpg.method.nameExact("bar").filename("Test0.swift").l
      barMethod.block.local.name.l should contain("a")
      val fieldAccesses = barMethod.block.ast.isCall.nameExact(Operators.fieldAccess).l
      fieldAccesses.code.l should contain("set.c")
    }

    "testMacroDecl" in {
      val cpg = code("""
      |macro m1(): Int = A.M1
      |macro m2(_: Int) = A.M2
      |macro m3(a b: Int) -> Int = A.M3
      |macro m4<T>(): T = A.M4 where T.Assoc: P
      |macro m5<T: P>(_: T)
      |""".stripMargin)
      // Macro declarations are not lowered into structured AST yet — they surface as Unknown
      // nodes (one per parser fragment) under <global> and produce no TypeDecls.
      val List(globalMethod) = cpg.method.nameExact("<global>").filename("Test0.swift").l
      val unknowns           = globalMethod.block.astChildren.collectAll[Unknown].l
      unknowns.code.l shouldBe List(
        "macro m1()",
        ": Int = A.M1",
        "macro m2(_: Int) = A.M2",
        "macro m3(a b: Int) -> Int = A.M3",
        "macro m4<T>()",
        ": T = A.M4 where T.Assoc: P",
        "macro m5<T: P>(_: T)"
      )
      globalMethod.block.astChildren.label.toSet shouldBe Set("UNKNOWN")
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").l shouldBe empty
    }

    "testClassWithPrivateSet" in {
      val cpg = code("""
      |struct Properties {
      |  class private(set) var privateSetterCustomNames: Bool
      |}
      |""".stripMargin)
      val List(props) = cpg.typeDecl.nameExact("Properties").l
      props.member.name.l shouldBe List("privateSetterCustomNames")
      props.member.typeFullName.l shouldBe List("Swift.Bool")
    }

    "testOpenVarInCodeBlockItemList" in {
      val cpg = code("""
      |func test() {
      |  open var foo = 2
      |}
      |""".stripMargin)
      val List(testMethod) = cpg.method.nameExact("test").filename("Test0.swift").l
      testMethod.block.local.name.l should contain("foo")
    }

    "testAsyncLetInLocalContext" in {
      val cpg = code("""
      |func foo() async {
      |  async let x: String = "x"
      |}
      |""".stripMargin)
      val List(fooMethod) = cpg.method.nameExact("foo").filename("Test0.swift").l
      val List(localX)    = fooMethod.block.local.nameExact("x").l
      localX.typeFullName shouldBe "Swift.String"
    }

    "testBorrowingConsumingParameterSpecifiers1" in {
      val cpg = code("""
      |struct borrowing {}
      |struct consuming {}
      |struct Foo {}
      |func foo(x: borrowing Foo) {}
      |func bar(x: consuming Foo) {}
      |func baz(x: (borrowing Foo, consuming Foo) -> ()) {}
      |""".stripMargin)
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf (
        "borrowing",
        "consuming",
        "Foo"
      )
      cpg.method.filename("Test0.swift").nameNot("<global>").name.l should contain allOf ("foo", "bar", "baz")
    }

    "testBorrowingConsumingParameterSpecifiers2" in {
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
      cpg.method.filename("Test0.swift").nameNot("<global>").name.l should contain allOf (
        "zim",
        "zang",
        "zung",
        "zip",
        "zap",
        "zoop"
      )
    }

    "testSuppressedImplicitConformance" in {
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
      cpg.typeDecl.filename("Test0.swift").nameNot("<global>").name.l should contain allOf (
        "Hello",
        "X",
        "Y",
        "Z1",
        "Z2",
        "Whatever"
      )
      val List(whatever) = cpg.typeDecl.nameExact("Whatever").l
      whatever.inheritsFromTypeFullName.l should contain("Equatable")
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
      arrayCall.typeFullName shouldBe Defines.Any

      val List(fooIdent) = assignment.astChildren.isIdentifier.l
      fooIdent.name shouldBe "foo"
      fooIdent.typeFullName shouldBe Defines.Array
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
      fooIdent.typeFullName shouldBe "Set"
    }

    "testDictionaryDeclaration" in {
      val cpg               = code("let x = [\"a\": 1, \"b\": 2]")
      val List(methodBlock) = cpg.method.nameExact("<global>").astChildren.isBlock.l
      val List(localX)      = methodBlock.local.nameExact("x").l
      val List(assignment)  = methodBlock.astChildren.isCall.l
      val List(identifierX) = assignment.astChildren.isIdentifier.l

      val List(localXViaRef) = identifierX.refOut.l
      localXViaRef shouldBe localX

      val List(block) = assignment.astChildren.isBlock.l
      checkObjectInitialization(block, ("\"a\"", "1"))
      checkObjectInitialization(block, ("\"b\"", "2"))
    }

    "testAddDictionaryElements" in {
      val cpg = code("""
        |var elements = ["A": "1", "B": "2"]
        |elements["A"] = "3"
        |print(elements["A"])
        |""".stripMargin)
      val List(tmpAccess1, tmpAccess2, elementsAccess1, elementsAccess2) = cpg.call(Operators.indexAccess).l
      tmpAccess1.code shouldBe "<tmp>0[\"A\"]"
      tmpAccess2.code shouldBe "<tmp>0[\"B\"]"

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

    "testInitAccessorsWithDefaultValues" in {
      val cpg = code("""
      |struct Test {
      |  var pair: (Int, Int) = (42, 0) {
      |    init(initialValue) {}
      |    get { (0, 42) }
      |     set { }
      |  }
      |}
      |""".stripMargin)
      val List(test) = cpg.typeDecl.nameExact("Test").l
      test.member.name.l shouldBe List("pair")
      test.method.name.l should contain("init")
    }

    "testBorrowingGetAccessor" in {
      val cpg = code("""
      |struct Foo {
      |  var x: Int {
      |    borrowing get {}
      |  }
      |}
      |""".stripMargin)
      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.member.name.l shouldBe List("x")
      foo.method.name.l should contain("x.getter")
    }

    "testLiteralInitializerWithTrailingClosure" in {
      val cpg               = code("let foo = 1 { return 1 }")
      val List(globalBlock) = cpg.method.nameExact("<global>").filename("Test0.swift").block.l
      globalBlock.local.name.l should contain("foo")
    }

  }

  private def checkObjectInitialization(node: Block, keyValuePair: (String, String)): Unit = {
    val (key, value) = keyValuePair

    val List(localTmp) = node.astChildren.isLocal.nameExact("<tmp>0").l
    localTmp.order shouldBe 0

    val List(tmp) = node.astChildren.isIdentifier.nameExact("<tmp>0").l
    tmp.code shouldBe "<tmp>0"

    val List(assignmentCall) = node.astChildren.isCall.codeExact(s"<tmp>0[$key] = $value").l
    assignmentCall.methodFullName shouldBe Operators.assignment

    val List(tmpAccess) = assignmentCall.argument(1).start.isCall.l
    tmpAccess.code shouldBe s"<tmp>0[$key]"
    tmpAccess.methodFullName shouldBe Operators.indexAccess
    tmpAccess.argumentIndex shouldBe 1
    val valueNode = assignmentCall.argument(2)
    valueNode.code shouldBe value

    val List(leftHandSideTmpId) = tmpAccess.astChildren.isIdentifier.nameExact("<tmp>0").l
    leftHandSideTmpId.argumentIndex shouldBe 1
    leftHandSideTmpId.code shouldBe "<tmp>0"

    val List(keyNode) = tmpAccess.astChildren.isLiteral.l
    keyNode.argumentIndex shouldBe 2
    keyNode.code shouldBe key
  }
}
