package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodParameterTests extends FuzzyCCodeToCpgSuite {

  override val code =
    """
      |  int main(int argc, char **argv) {
      | }""".stripMargin

  "should return exactly two parameters with correct fields" in {
    cpg.parameter.name.toSet shouldBe Set("argc", "argv")

    val List(x) = cpg.parameter.name("argc").l
    x.code shouldBe "int argc"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Some(2)
    x.columnNumber shouldBe Some(11)
    x.order shouldBe 1

    val List(y) = cpg.parameter.name("argv").l
    y.code shouldBe "char **argv"
    y.typeFullName shouldBe "char * *"
    y.lineNumber shouldBe Some(2)
    y.columnNumber shouldBe Some(21)
    y.order shouldBe 2
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("argc").method.name.l shouldBe List("main")
  }

}
