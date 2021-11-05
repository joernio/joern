package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.FileTraversal

import java.io.{File => JFile}

class TypeDeclTests extends JimpleCodeToCpgFixture {

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
    x.filename.startsWith(JFile.separator) shouldBe true
    x.filename.endsWith(".class") shouldBe true
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
