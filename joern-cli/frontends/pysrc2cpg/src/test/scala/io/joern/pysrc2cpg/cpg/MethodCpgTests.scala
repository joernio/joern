package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.Py2CpgTestContext
import io.shiftleft.semanticcpg.language.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodCpgTests extends AnyFreeSpec with Matchers {
  "A method" - {

    lazy val cpg = Py2CpgTestContext.buildCpg(
      """
        |def method():
        |   pass
        |""".stripMargin,
      "a/b.py"
    )

    "test method full name" in {
      val method = cpg.method.name("method").head
      method.fullName shouldBe "a/b.py:<module>.method"
    }
  }

  "test method redefinition" in {
    val cpg = Py2CpgTestContext.buildCpg(
      """
        |class Foo():
        |  def method():
        |    pass
        |  def method():
        |    pass
        |  def method():
        |    pass
        |""".stripMargin,
      "a/b.py"
    )

    cpg.method.name("method").map(m => (m.name, m.fullName)).l should contain theSameElementsAs (List(
      ("method", "a/b.py:<module>.Foo.method"),
      ("method", "a/b.py:<module>.Foo.method$redefinition1"),
      ("method", "a/b.py:<module>.Foo.method$redefinition2")
    ))

    cpg.typeDecl.name("Foo").member.name("method").dynamicTypeHintFullName.l should contain theSameElementsAs (
      List("a/b.py:<module>.Foo.method$redefinition2")
    )

    cpg.typeDecl.name("Foo<meta>").member.name("method").dynamicTypeHintFullName.l should contain theSameElementsAs (
      List("a/b.py:<module>.Foo.method<metaClassAdapter>")
    )
  }

}
