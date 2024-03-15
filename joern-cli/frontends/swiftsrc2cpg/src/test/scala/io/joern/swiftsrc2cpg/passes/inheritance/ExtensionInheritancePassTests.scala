package io.joern.swiftsrc2cpg.passes.inheritance

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.File.PropertyDefaults
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
        code(classFooCode, "projectA/Source/Foo.swift")
          .moreCode(classBarCode, "main/Bar.swift")
          .moreCode(extensionBarCode, "extensions/Bar+Ext.swift")
          .moreCode(classFooCode, "projectB/Source/Foo.swift")
          .moreCode(extension1FooCode, "projectA/Source/Foo+Ext1.swift")
          .moreCode(extension2FooCode, "projectA/Source/Foo+Ext2.swift")
          .moreCode(extension1FooCode, "projectB/Source/Foo+Ext1.swift")
          .moreCode(extensionStringCode, "String+Ext.swift")

      val List(fooTypeDeclInProjectA) = cpg.typeDecl.fullNameExact("projectA/Source/Foo.swift:<global>:Foo").l
      fooTypeDeclInProjectA.inheritsFromTypeFullName.sorted.l shouldBe List(
        "Bar",
        "projectA/Source/Foo+Ext1.swift:<global>:Foo<extension>",
        "projectA/Source/Foo+Ext2.swift:<global>:Foo<extension>"
      )

      val List(fooTypeDeclInProjectB) = cpg.typeDecl.fullNameExact("projectB/Source/Foo.swift:<global>:Foo").l
      fooTypeDeclInProjectB.inheritsFromTypeFullName.sorted.l shouldBe List(
        "Bar",
        "projectB/Source/Foo+Ext1.swift:<global>:Foo<extension>"
      )

      val List(stringTypeDecl) = cpg.typeDecl("String").l
      stringTypeDecl.inheritsFromTypeFullName.l shouldBe List("String+Ext.swift:<global>:String<extension>")

      val List(barTypeDeclInMain) = cpg.typeDecl.fullNameExact("main/Bar.swift:<global>:Bar").l
      barTypeDeclInMain.inheritsFromTypeFullName.sorted.l shouldBe List(
        "extensions/Bar+Ext.swift:<global>:Bar<extension>"
      )
    }

  }

}
