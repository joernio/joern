package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class MethodReturnTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """contract Foo {
      |  function foo() public returns (int) { return 1; }
      |}
      |""".stripMargin

  /*
    Under the method node, it should look as follows:
    [MethodParameterIn(THIS), Modifier(public), Block(), MethodReturn(int)]
   */

  "should have METHOD_RETURN node with correct fields" in {
//    println(cpg.method.name("foo").methodReturn.typeFullName.l)
    val List(x) = cpg.method.name("foo").methodReturn.typeFullName("int").l
    x.code shouldBe "int"
    x.typeFullName shouldBe "int"
//    x.lineNumber shouldBe Some(1)
    // we expect the METHOD_RETURN node to be the right-most
    // child so that when traversing the AST from left to
    // right in CFG construction, we visit it last.
    x.order shouldBe 2
  }

  "should have a RETURN node with correct fields" in {
    val List(x) = cpg.method.name("foo").ast.isReturn.l
    x.code shouldBe "return 1;"
    x.order shouldBe 1
    x.argumentIndex shouldBe 1
    x.astChildren.size shouldBe 1
    x.argumentOut.size shouldBe 1
  }

  "should allow traversing to method" in {
    cpg.methodReturn.code("int").method.name.l shouldBe List("foo")
  }

}
