package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class MethodReturnTests extends FuzzyCCodeToCpgSuite {

  override val code =
    """
      | int *foo() { return x; }
      |""".stripMargin

  "should have METHOD_RETURN node with correct fields" in {
    val List(x) = cpg.methodReturn.l
    x.code shouldBe "int *"
    x.typeFullName shouldBe "int *"
    x.lineNumber shouldBe Some(2)
    x.columnNumber shouldBe Some(1)
    // we expect the METHOD_RETURN node to be the right-most
    // child so that when traversing the AST from left to
    // right in CFG construction, we visit it last.
    x.order shouldBe 2
  }

  "should allow traversing to method" in {
    cpg.methodReturn.method.name.l shouldBe List("foo")
  }

}
