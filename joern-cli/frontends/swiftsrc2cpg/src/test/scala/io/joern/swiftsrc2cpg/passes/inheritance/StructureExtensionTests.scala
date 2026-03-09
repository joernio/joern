package io.joern.swiftsrc2cpg.passes.inheritance

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Assertion

class StructureExtensionTests extends SwiftSrc2CpgSuite {

  private val structACode =
    """
      |public struct A {}
      |""".stripMargin

  private val structBCode =
    """
      |private struct B {
      |  var b = 0.0
      |}
      |""".stripMargin

  private val structFooCode =
    """
      |struct Foo: Bar { // implicitly internal (private)
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

  private val structExtensionCode =
    """
      |extension Foo: SomeProtocol, AnotherProtocol {
      |  func someOtherFunc() {}
      |}
      |""".stripMargin

  private def testStructExtension(cpg: Cpg): Assertion = {
    val List(typeDeclA) = cpg.typeDecl.nameExact("A").l
    typeDeclA.fullName shouldBe "Test0.swift:<global>.A"
    typeDeclA.member.l shouldBe empty
    typeDeclA.inheritsFromTypeFullName.l shouldBe empty
    typeDeclA.modifier.modifierType.l shouldBe List(ModifierTypes.PUBLIC)

    val List(typeDeclB) = cpg.typeDecl.nameExact("B").l
    typeDeclB.fullName shouldBe "Test0.swift:<global>.B"
    typeDeclB.member.name.l shouldBe List("b")
    typeDeclB.inheritsFromTypeFullName.l shouldBe empty
    typeDeclB.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)
    val List(bConstructor) = typeDeclB.method.isConstructor.l
    bConstructor.fullName shouldBe s"Test0.swift:<global>.B.init:()->Test0.swift:<global>.B"
    bConstructor.block.astChildren.code.l shouldBe List("var b = 0.0")

    val List(typeDeclFoo) = cpg.typeDecl.nameExact("Foo").l
    typeDeclFoo.fullName shouldBe "Test0.swift:<global>.Foo"
    typeDeclFoo.member.name.l.sorted shouldBe List("a", "b", "c", "d", "e", "f", "g")

    /** TODO: Re-enable once extension methods are properly accessible via EXTENSION_BLOCK
      * typeDeclFoo.boundMethod.fullName.l shouldBe List(
      * "Test0.swift:<global>.Foo.init:(paramA:Swift.String,paramB:Swift.Int)->Test0.swift:<global>.Foo",
      * "Test0.swift:<global>.Foo.someFunc:()->ANY", "Test0.swift:<global>.Foo.someMethod:()->ANY",
      * "Test0.swift:<global>.Foo.square:()->ANY", "Test0.swift:<global>.Foo.<clinit>:()->Test0.swift:<global>.Foo",
      * "Test0.swift:<global>.Foo<extension>.someOtherFunc:()->ANY" )
      */

    cpg.typ.name.l.distinct shouldBe cpg.typ.name.l
    typeDeclFoo.inheritsFromTypeFullName.sorted.l shouldBe List("AnotherProtocol", "Bar", "SomeProtocol")
    typeDeclFoo.modifier.modifierType.l shouldBe List(ModifierTypes.PRIVATE)

    val List(fooConstructor) = typeDeclFoo.method.nameExact("init").isConstructor.l
    fooConstructor.fullName shouldBe "Test0.swift:<global>.Foo.init:(paramA:Swift.String,paramB:Swift.Int)->Test0.swift:<global>.Foo"
    fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List(
      "var a = A()",
      "var b = false",
      "var c = 0.0"
    )

    val List(fooStaticInit) = typeDeclFoo.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).isConstructor.l
    fooStaticInit.fullName shouldBe s"Test0.swift:<global>.Foo.${io.joern.x2cpg.Defines.StaticInitMethodName}:()->Test0.swift:<global>.Foo"
    fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var e = 1", "var f = true")

    val List(someOtherFunc) = cpg.method.nameExact("someOtherFunc").l
    someOtherFunc.fullName shouldBe "Test0.swift:<global>.Foo<extension>.someOtherFunc:()->ANY"
  }

  "StructureExtensionTests" should {

    "test Structure and Extension defined afterwards" in {
      val cpg = code(s"""
        |$structACode
        |$structBCode
        |$structFooCode
        |$structExtensionCode
        |""".stripMargin)
      testStructExtension(cpg)
    }

    "test Structure and Extension defined beforehand" in {
      val cpg = code(s"""
        |$structACode
        |$structBCode
        |$structExtensionCode
        |$structFooCode
        |""".stripMargin)
      testStructExtension(cpg)
    }

  }

}
