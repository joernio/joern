package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class EnumerationExtensionTests extends AbstractPassTest {

  "EnumerationExtensionTests" should {

    "test Enumeration and Extension defined afterwards" in AstFixture("""
        |public enum A {}
        |private enum B {
        |  var b = 0.0
        |}
        |
        |enum Foo: Bar { // implicitly internal (private)
        |  case tuple(Int, Int, Int, Int)
        |  case c1 = 1, c2, c3
        |  indirect case c4(Foo)
        |
        |  public var a = A()
        |  private var b = false
        |  var c = 0.0
        |  var d: String?
        |
        |  static var e = 1
        |  static var f = true
        |
        |  var g: Double { return self * 1_000.0 }
        |
        |  init(paramA: String, paramB: Int) {
        |    self.init()
        |  }
        |
        |  private func someFunc() {}
        |
        |  override internal func someMethod() {
        |    super.someMethod()
        |  }
        |
        |  mutating func square() {
        |    self = self * self
        |  }
        |
        |}
        |
        |extension Foo: SomeProtocol, AnotherProtocol {
        |  var h = 0.0
        |  var i: String
        |  static var j = 2
        |  func someOtherFunc() {}
        |}
        |
        |""".stripMargin) { cpg =>
      val List(typeDeclA) = cpg.typeDecl.nameExact("A").l
      typeDeclA.fullName shouldBe "code.swift:<global>:A"
      typeDeclA.member.l shouldBe empty
      typeDeclA.inheritsFromTypeFullName.l shouldBe empty
      typeDeclA.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

      val List(typeDeclB) = cpg.typeDecl.nameExact("B").l
      typeDeclB.fullName shouldBe "code.swift:<global>:B"
      typeDeclB.member.name.l shouldBe List("b")
      typeDeclB.inheritsFromTypeFullName.l shouldBe empty
      typeDeclB.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      val List(bConstructor) = typeDeclB.method.isConstructor.l
      bConstructor.fullName shouldBe s"code.swift:<global>:B:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      bConstructor.block.astChildren.code.l shouldBe List("var b = 0.0")

      val List(typeDeclFoo) = cpg.typeDecl.nameExact("Foo").l
      typeDeclFoo.fullName shouldBe "code.swift:<global>:Foo"
      typeDeclFoo.member.name.l.sorted shouldBe List(
        "a",
        "b",
        "c",
        "c1",
        "c2",
        "c3",
        "c4",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "someFunc",
        "someMethod",
        "someOtherFunc",
        "square",
        "tuple"
      )
      typeDeclFoo.inheritsFromTypeFullName.l shouldBe List("AnotherProtocol", "Bar", "SomeProtocol")
      typeDeclFoo.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)

      val List(fooConstructor) = typeDeclFoo.method.isConstructor.l
      fooConstructor.fullName shouldBe "code.swift:<global>:Foo:init"
      fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List(
        "c1 = 1",
        "var a = A()",
        "var b = false",
        "var c = 0.0",
        "var g: Double { return self * 1_000.0 }", // lowered as assignment
        "var h = 0.0"
      )

      val List(fooStaticInit) = typeDeclFoo.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      fooStaticInit.fullName shouldBe s"code.swift:<global>:Foo:${io.joern.x2cpg.Defines.StaticInitMethodName}"
      fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var e = 1", "var f = true", "var j = 2")
    }

    "test Structure and Extension defined beforehand" in AstFixture("""
        |public enum A {}
        |private enum B {
        |  var b = 0.0
        |}
        |
        |extension Foo: SomeProtocol, AnotherProtocol {
        |  var h = 0.0
        |  var i: String
        |  static var j = 2
        |  func someOtherFunc() {}
        |}
        |
        |enum Foo: Bar { // implicitly internal (private)
        |  case tuple(Int, Int, Int, Int)
        |  case c1 = 1, c2, c3
        |  indirect case c4(Foo)
        |  
        |  public var a = A()
        |  private var b = false
        |  var c = 0.0
        |  var d: String?
        |
        |  static var e = 1
        |  static var f = true
        |
        |  var g: Double { return self * 1_000.0 }
        |
        |  init(paramA: String, paramB: Int) {
        |    self.init()
        |  }
        |
        |  private func someFunc() {}
        |
        |  override internal func someMethod() {
        |    super.someMethod()
        |  }
        |
        |  mutating func square() {
        |    self = self * self
        |  }
        |
        |}
        |""".stripMargin) { cpg =>
      val List(typeDeclA) = cpg.typeDecl.nameExact("A").l
      typeDeclA.fullName shouldBe "code.swift:<global>:A"
      typeDeclA.member.l shouldBe empty
      typeDeclA.inheritsFromTypeFullName.l shouldBe empty
      typeDeclA.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

      val List(typeDeclB) = cpg.typeDecl.nameExact("B").l
      typeDeclB.fullName shouldBe "code.swift:<global>:B"
      typeDeclB.member.name.l shouldBe List("b")
      typeDeclB.inheritsFromTypeFullName.l shouldBe empty
      typeDeclB.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
      val List(bConstructor) = typeDeclB.method.isConstructor.l
      bConstructor.fullName shouldBe s"code.swift:<global>:B:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      bConstructor.block.astChildren.code.l shouldBe List("var b = 0.0")

      val List(typeDeclFoo) = cpg.typeDecl.nameExact("Foo").l
      typeDeclFoo.fullName shouldBe "code.swift:<global>:Foo"
      typeDeclFoo.member.name.l.sorted shouldBe List(
        "a",
        "b",
        "c",
        "c1",
        "c2",
        "c3",
        "c4",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "someFunc",
        "someMethod",
        "someOtherFunc",
        "square",
        "tuple"
      )
      typeDeclFoo.inheritsFromTypeFullName.l shouldBe List("AnotherProtocol", "Bar", "SomeProtocol")
      typeDeclFoo.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)

      val List(fooFakeConstructor) =
        typeDeclFoo.method.isConstructor.nameExact(io.joern.x2cpg.Defines.ConstructorMethodName).l
      fooFakeConstructor.fullName shouldBe s"code.swift:<global>:Foo:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      fooFakeConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var h = 0.0")

      val List(fooConstructor) = typeDeclFoo.method.isConstructor.nameExact("init").l
      fooConstructor.fullName shouldBe "code.swift:<global>:Foo:init"
      fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List(
        "c1 = 1",
        "var a = A()",
        "var b = false",
        "var c = 0.0",
        "var g: Double { return self * 1_000.0 }"
      )

      val List(fooStaticInit) = typeDeclFoo.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      fooStaticInit.fullName shouldBe s"code.swift:<global>:Foo:${io.joern.x2cpg.Defines.StaticInitMethodName}"
      fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var e = 1", "var f = true", "var j = 2")
    }

  }

}
