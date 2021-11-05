package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

import java.io.File

class MethodTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """ class Foo {
      |   int foo(int param1, int param2) {
      |     return 1;
      |   }
      | }
      |""".stripMargin

  "should contain exactly one non-stub method node with correct fields" in {
    val List(x) = cpg.method.nameNot("<init>").isExternal(false).l
    x.name shouldBe "foo"
    x.fullName shouldBe "Foo.foo:int(int,int)"
    x.code shouldBe "int foo(int param1, int param2)"
    x.signature shouldBe "int(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.filename.startsWith(File.separator) shouldBe true
    x.filename.endsWith(".class") shouldBe true
    x.lineNumber shouldBe Some(2)
    x.columnNumber shouldBe Some(-1)
  }

//  "should return correct number of lines" in {
//    cpg.method.name("foo").numberOfLines.l shouldBe List(3)
//  }

  "should allow traversing to parameters" in {
    cpg.method.name("foo").parameter.name.toSet shouldBe Set("param1", "param2")
  }

  "should allow traversing to methodReturn" in {
    cpg.method.name("foo").methodReturn.typeFullName.l shouldBe List("int")
  }

  "should allow traversing to file" in {
    cpg.method.name("foo").file.name.l should not be empty
  }

}
