package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

class LocalTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      | class Foo {
      |   int foo() {
      |     int x;
      |     Integer y = null;
      |   }
      | }
      |""".stripMargin

  "should contain locals `x` and `y` with correct fields set" in {
    val List(x: Local) = cpg.local("x").l
    val List(y: Local) = cpg.local("y").l
    x.name shouldBe "x"
    x.code shouldBe "int x"
    x.typeFullName shouldBe "int"
    x.order shouldBe 1

    y.name shouldBe "y"
    y.code shouldBe "Integer y"
    y.typeFullName shouldBe "java.lang.Integer"
    y.order shouldBe 2
  }
}
