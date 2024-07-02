package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class MethodParameterTests extends C2CpgSuite {

  private val cpg = code("""
      |  int main(int argc, char **argv) {
      | }""".stripMargin)

  "should return exactly two parameters with correct fields" in {
    cpg.parameter.name.toSetMutable shouldBe Set("argc", "argv")

    val List(x) = cpg.parameter.name("argc").l
    x.code shouldBe "int argc"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Option(2)
    x.columnNumber shouldBe Option(12)
    x.order shouldBe 1

    val List(y) = cpg.parameter.name("argv").l
    y.code shouldBe "char **argv"
    y.typeFullName shouldBe "char**"
    y.lineNumber shouldBe Option(2)
    y.columnNumber shouldBe Option(22)
    y.order shouldBe 2
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("argc").method.name.l shouldBe List("main")
  }

}
