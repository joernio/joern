package io.joern.c2cpg.standard

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodTests extends CCodeToCpgSuite {

  override val code: String =
    """
      |  int main(int argc, char **argv) {
      | }""".stripMargin

  "should contain exactly one method node with correct fields" in {
    val List(x) = cpg.method.name("main").l
    x.name shouldBe "main"
    x.fullName shouldBe "main"
    x.code shouldBe "int main (int argc,char **argv)"
    x.signature shouldBe "int main (int,char**)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.filename.startsWith("/") shouldBe true
    x.lineNumber shouldBe Some(2)
    x.lineNumberEnd shouldBe Some(3)
    x.columnNumber shouldBe Some(2)
    x.columnNumberEnd shouldBe Some(2)
  }

  "should return correct number of lines" in {
    cpg.method.name("main").numberOfLines.l shouldBe List(2)
  }

  "should allow traversing to parameters" in {
    cpg.method.name("main").parameter.name.toSet shouldBe Set("argc", "argv")
  }

  "should allow traversing to methodReturn" in {
    cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("int")
  }

  "should allow traversing to file" in {
    cpg.method.name("main").file.name.l should not be empty
  }

}

class CMethodTests2 extends CCodeToCpgSuite {
  override val code = "int foo(); int bar() { return woo(); }"

  "should identify method as stub" in {
    cpg.method.isStub.name.toSet shouldBe Set("<global>", "foo", "woo")
    cpg.method.isNotStub.name.l shouldBe List("bar")
  }
}
