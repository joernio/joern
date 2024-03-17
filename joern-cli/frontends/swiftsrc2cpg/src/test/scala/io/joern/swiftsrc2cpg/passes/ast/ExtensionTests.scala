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
      |  var d = 0.0
      |  var e: String
      |  static var f = 3
      |  func someOtherFunc() {}
      |}
      |""".stripMargin

  private val ext2Code =
    """
      |extension Foo : Bar {
      |  var g: String = ""
      |}
      |""".stripMargin

  private val cpg = code(fooCode, "Foo.swift").moreCode(ext1Code, "Ext1.swift").moreCode(ext2Code, "Ext2.swift")

  "ExtensionTests" should {

    "create stable ASTs from multiple files" in {
      val List(fooTypeDecl) = cpg.typeDecl.fullNameExact("Foo.swift:<global>:Foo").l
      fooTypeDecl.name shouldBe "Foo"
      fooTypeDecl.member.name.l.sorted shouldBe List("a", "b", "c", "someFunc")
      val List(fooConstructor) = fooTypeDecl.method.isConstructor.l
      fooConstructor.fullName shouldBe s"Foo.swift:<global>:Foo:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      fooConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var a = 1")
      val List(fooStaticInit) = fooTypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      fooStaticInit.fullName shouldBe s"Foo.swift:<global>:Foo:${io.joern.x2cpg.Defines.StaticInitMethodName}"
      fooStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var c = 2")

      val List(fooExt1TypeDecl) = cpg.typeDecl.fullNameExact("Ext1.swift:<global>:Foo<extension>").l
      fooExt1TypeDecl.name shouldBe "Foo<extension>"
      fooExt1TypeDecl.member.name.l.sorted shouldBe List("d", "e", "f", "someOtherFunc")
      val List(fooExt1TypeDeclConstructor) = fooExt1TypeDecl.method.isConstructor.l
      fooExt1TypeDeclConstructor.fullName shouldBe s"Ext1.swift:<global>:Foo<extension>:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      fooExt1TypeDeclConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var d = 0.0")
      val List(fooExt1TypeDeclStaticInit) =
        fooExt1TypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      fooExt1TypeDeclStaticInit.fullName shouldBe s"Ext1.swift:<global>:Foo<extension>:${io.joern.x2cpg.Defines.StaticInitMethodName}"
      fooExt1TypeDeclStaticInit.block.astChildren.assignment.code.l.sorted shouldBe List("var f = 3")

      val List(fooExt2TypeDecl) = cpg.typeDecl.fullNameExact("Ext2.swift:<global>:Foo<extension>").l
      fooExt2TypeDecl.name shouldBe "Foo<extension>"
      fooExt2TypeDecl.member.name.l.sorted shouldBe List("g")
      val List(fooExt2TypeDeclConstructor) = fooExt2TypeDecl.method.isConstructor.l
      fooExt2TypeDeclConstructor.fullName shouldBe s"Ext2.swift:<global>:Foo<extension>:${io.joern.x2cpg.Defines.ConstructorMethodName}"
      fooExt2TypeDeclConstructor.block.astChildren.assignment.code.l.sorted shouldBe List("var g: String = \"\"")
      fooExt2TypeDecl.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName) shouldBe empty
    }

  }

}
