package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.semanticcpg.language.*

class ExtensionTests extends AstSwiftSrc2CpgSuite {

  private val fooCode = """
     |class Foo {
     |  var a = 1
     |  var b: String
     |  static var c = 2
     |  func someFunc() {}
     |}""".stripMargin

  private val ext1Code =
    """
      |extension Foo {
      |  var d: Int { return 1 }
      |
      |  func someFooFunc() {}
      |}
      |""".stripMargin

  private val ext2Code =
    """
      |extension Foo : Bar {
      |  var e: String { return "hello" }
      |
      |  func someOtherFooFunc() {}
      |}
      |""".stripMargin

  private val cpg = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift").moreCode(ext2Code, "Ext2.swift")

  "ExtensionTests" should {

    "create stable ASTs from multiple files" in {
      val List(fooTypeDecl) = cpg.typeDecl.fullNameExact("Foo.swift:<global>.Foo").l
      fooTypeDecl.name shouldBe "Foo"
      val List(fooConstructor) = fooTypeDecl.method.nameExact("init").isConstructor.l
      fooConstructor.fullName shouldBe s"Foo.swift:<global>.Foo.init:()->Foo.swift:<global>.Foo"
      fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var a = 1")
      val List(fooStaticInit) =
        fooTypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).isConstructor.l
      fooStaticInit.fullName shouldBe s"Foo.swift:<global>.Foo.${io.joern.x2cpg.Defines.StaticInitMethodName}:()->Foo.swift:<global>.Foo"
      fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var c = 2")

      val List(someFooFunc) = cpg.method.nameExact("someFooFunc").l
      someFooFunc.fullName shouldBe "Ext1.swift:<global>.Foo<extension>.someFooFunc:()->ANY"

      val List(someOtherFooFunc) = cpg.method.nameExact("someOtherFooFunc").l
      someOtherFooFunc.fullName shouldBe "Ext2.swift:<global>.Foo<extension>.someOtherFooFunc:()->ANY"

      val Seq(a, b, c, d, e) = fooTypeDecl.member.sortBy(_.name)
      a.name shouldBe "a"
      b.name shouldBe "b"
      c.name shouldBe "c"
      d.name shouldBe "d"
      e.name shouldBe "e"
      a.typeFullName shouldBe "ANY" // we don't have type inference without compiler support
      b.typeFullName shouldBe "Swift.String"
      c.typeFullName shouldBe "ANY" // we don't have type inference without compiler support
      d.typeFullName shouldBe "Swift.Int"
      e.typeFullName shouldBe "Swift.String"
    }

  }

}
