package io.joern.jimple2cpg.querying
import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

import java.io.{File => JFile}

class InterfaceTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """
      |interface Foo {
      |
      |   int add(int x, int y);
      |
      |}
      |""".stripMargin

  "should contain a type decl for `Foo` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").l
    x.name shouldBe "Foo"
    x.code shouldBe "Foo"
    x.fullName shouldBe "Foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("java.lang.Object")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename.startsWith(JFile.separator) shouldBe true
    x.filename.endsWith(".class") shouldBe true
  }

  "should contain a method for `Foo.add` with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").method.l
    x.name shouldBe "add"
    x.code shouldBe "int add(int param1, int param2)"
    x.fullName shouldBe "Foo.add:int(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
  }

}
