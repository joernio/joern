package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ContentCpgTests extends PySrc2CpgFixture() {
  "Content" should {
    val typeDeclContent =
      """|class Foo():
         |  pass
         |""".stripMargin
    val methodContent =
      """|def bar():
         |  pass
         |""".stripMargin
    val content =
      s"""
        |$typeDeclContent$methodContent""".stripMargin
    val cpg = code(content)

    "be set on file node" in {
      cpg.file.content.head shouldBe content
    }

    "be correct on type decl" in {
      cpg.typeDecl.name("Foo").content.head shouldBe typeDeclContent
    }

    "be correct on method" in {
      cpg.method.name("bar").content.head shouldBe methodContent
    }
  }

  "Extra content checks" should {
    "be correct method followed be EOF" in {
      val methodContent =
        """|def bar():
           |  pass""".stripMargin
      val cpg = code(methodContent)
      cpg.method.name("bar").content.head shouldBe methodContent
    }

    "be correct method followed be new line" in {
      val methodContent =
        """|def bar():
           |  pass
           |""".stripMargin
      val cpg = code(methodContent)
      cpg.method.name("bar").content.head shouldBe methodContent
    }
  }

}
