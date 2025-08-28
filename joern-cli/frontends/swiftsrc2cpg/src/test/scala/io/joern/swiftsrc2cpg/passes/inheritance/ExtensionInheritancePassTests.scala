package io.joern.swiftsrc2cpg.passes.inheritance

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class ExtensionInheritancePassTests extends SwiftSrc2CpgSuite {

  private val classBarCode =
    """
      |class Bar {
      |}""".stripMargin

  private val extensionBarCode =
    """
      |extension Bar {
      |}""".stripMargin

  private val classFooCode = """
     |class Foo : Bar {
     |}""".stripMargin

  private val extension1FooCode =
    """
      |extension Foo {
      |  var name = "1"
      |}""".stripMargin

  private val extension2FooCode =
    """
      |extension Foo {
      |  var name = "2"
      |}""".stripMargin

  private val extensionStringCode =
    """
      |extension String {
      |  func toUpperCase -> String {
      |    // ...
      |  }
      |}""".stripMargin

  "ExtensionInheritancePass" should {

    "generate inheritance for extensions correctly" in {
      val cpg =
        code(classFooCode, "projectA/Sources/Foo.swift")
          .moreCode(classBarCode, "main/Bar.swift")
          .moreCode(extensionBarCode, "extensions/Bar+Ext.swift")
          .moreCode(classFooCode, "projectB/Sources/Foo.swift")
          .moreCode(extension1FooCode, "projectA/Sources/Foo+Ext1.swift")
          .moreCode(extension2FooCode, "projectA/Sources/Foo+Ext2.swift")
          .moreCode(extension1FooCode, "projectB/Sources/Foo+Ext1.swift")
          .moreCode(extensionStringCode, "String+Ext.swift")

      val List(fooTypeDeclInProjectA) = cpg.typeDecl.fullNameExact("projectA/Sources/Foo.swift:<global>.Foo").l
      fooTypeDeclInProjectA.inheritsFromTypeFullName.sorted.l shouldBe List(
        "Bar",
        "projectA/Sources/Foo+Ext1.swift:<global>.Foo<extension>",
        "projectA/Sources/Foo+Ext2.swift:<global>.Foo<extension>"
      )

      val List(fooTypeDeclInProjectB) = cpg.typeDecl.fullNameExact("projectB/Sources/Foo.swift:<global>.Foo").l
      fooTypeDeclInProjectB.inheritsFromTypeFullName.sorted.l shouldBe List(
        "Bar",
        "projectB/Sources/Foo+Ext1.swift:<global>.Foo<extension>"
      )

      val List(stringTypeDecl) = cpg.typeDecl.nameExact("Swift.String").l
      stringTypeDecl.inheritsFromTypeFullName.l shouldBe List("String+Ext.swift:<global>.String<extension>")

      val List(barTypeDeclInMain) = cpg.typeDecl.fullNameExact("main/Bar.swift:<global>.Bar").l
      barTypeDeclInMain.inheritsFromTypeFullName.sorted.l shouldBe List(
        "extensions/Bar+Ext.swift:<global>.Bar<extension>"
      )
    }

  }

}
