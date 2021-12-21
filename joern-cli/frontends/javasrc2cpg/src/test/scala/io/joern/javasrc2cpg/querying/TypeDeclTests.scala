package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.File

class TypeDeclTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      | package Foo;
      | class Bar extends Woo {
      |   int x;
      |   int method () { return 1; }
      | };
      | class Woo {}
      | """.stripMargin

  "should contain a type decl for `foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Bar").l
    x.name shouldBe "Bar"
    x.code shouldBe "Bar"
    x.fullName shouldBe "Foo.Bar"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("Foo.Woo")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:" // Windows
    )
    x.filename.endsWith(".java") shouldBe true
  }

  "should contain type decl for external type `int`" in {
    val List(x) = cpg.typeDecl("int").l
    x.name shouldBe "int"
    x.fullName shouldBe "int"
    x.isExternal shouldBe true
    x.inheritsFromTypeFullName shouldBe List()
    x.aliasTypeFullName shouldBe None
    x.order shouldBe -1
    x.filename shouldBe FileTraversal.UNKNOWN
  }

}
