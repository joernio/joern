package io.joern.swiftsrc2cpg.passes.inheritance

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Assertion

class EnumerationExtensionTests extends SwiftSrc2CpgSuite {

  private val enumACode =
    """
      |public enum A {}
      |""".stripMargin

  private val enumBCode =
    """
      |private enum B {
      |  var b = 0.0
      |}
      |""".stripMargin

  private val enumFooCode =
    """
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
      |}
      |""".stripMargin

  private val enumFooExtensionCode =
    """
      |extension Foo: SomeProtocol, AnotherProtocol {
      |  var h = 0.0
      |  var i: String
      |  static var j = 2
      |  func someOtherFunc() {}
      |}
      |""".stripMargin

  private def testEnumExtension(cpg: Cpg): Assertion = {
    val List(typeDeclA) = cpg.typeDecl.nameExact("A").l
    typeDeclA.fullName shouldBe "Test0.swift:<global>:A"
    typeDeclA.member.l shouldBe empty
    typeDeclA.inheritsFromTypeFullName.l shouldBe empty
    typeDeclA.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

    val List(typeDeclB) = cpg.typeDecl.nameExact("B").l
    typeDeclB.fullName shouldBe "Test0.swift:<global>:B"
    typeDeclB.member.name.l shouldBe List("b")
    typeDeclB.inheritsFromTypeFullName.l shouldBe empty
    typeDeclB.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
    val List(bConstructor) = typeDeclB.method.isConstructor.l
    bConstructor.fullName shouldBe s"Test0.swift:<global>:B:${io.joern.x2cpg.Defines.ConstructorMethodName}"
    bConstructor.block.astChildren.code.l shouldBe List("var b = 0.0")

    val List(typeDeclFoo) = cpg.typeDecl.nameExact("Foo").l
    typeDeclFoo.fullName shouldBe "Test0.swift:<global>:Foo"
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
      "someFunc",
      "someMethod",
      "square",
      "tuple"
    )
    typeDeclFoo.inheritsFromTypeFullName.l shouldBe List("Bar", "Test0.swift:<global>:Foo<extension>")
    typeDeclFoo.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)

    val List(fooConstructor) = typeDeclFoo.method.isConstructor.l
    fooConstructor.fullName shouldBe "Test0.swift:<global>:Foo:init:Test0.swift:<global>:Foo(String,Int)"
    fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List(
      "c1 = 1",
      "var a = A()",
      "var b = false",
      "var c = 0.0",
      "var g: Double { return self * 1_000.0 }" // lowered as assignment
    )

    val List(fooStaticInit) = typeDeclFoo.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
    fooStaticInit.fullName shouldBe s"Test0.swift:<global>:Foo:${io.joern.x2cpg.Defines.StaticInitMethodName}"
    fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var e = 1", "var f = true")

    val List(typeDeclFooExtension) = cpg.typeDecl.nameExact("Foo<extension>").l
    typeDeclFooExtension.fullName shouldBe "Test0.swift:<global>:Foo<extension>"
    typeDeclFooExtension.member.name.l.sorted shouldBe List("h", "i", "j", "someOtherFunc")
    typeDeclFooExtension.inheritsFromTypeFullName.l shouldBe List("AnotherProtocol", "SomeProtocol")
    typeDeclFooExtension.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)

    val List(fooExtensionConstructor) = typeDeclFooExtension.method.isConstructor.l
    fooExtensionConstructor.fullName shouldBe s"Test0.swift:<global>:Foo<extension>:${io.joern.x2cpg.Defines.ConstructorMethodName}"
    fooExtensionConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var h = 0.0")

    val List(fooExtensionStaticInit) =
      typeDeclFooExtension.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
    fooExtensionStaticInit.fullName shouldBe s"Test0.swift:<global>:Foo<extension>:${io.joern.x2cpg.Defines.StaticInitMethodName}"
    fooExtensionStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var j = 2")
  }

  "EnumerationExtensionTests" should {

    "test Enumeration and Extension defined afterwards" in {
      val cpg = code(s"""
        |$enumACode
        |$enumBCode
        |$enumFooCode
        |$enumFooExtensionCode
        |""".stripMargin)
      testEnumExtension(cpg)
    }

    "test Structure and Extension defined beforehand" in {
      val cpg = code(s"""
        |$enumACode
        |$enumBCode
        |$enumFooExtensionCode
        |$enumFooCode
        |""".stripMargin)
      testEnumExtension(cpg)
    }

  }

}
