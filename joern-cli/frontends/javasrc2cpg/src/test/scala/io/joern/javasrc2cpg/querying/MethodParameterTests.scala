package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

class MethodParameterTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """package a;
      |class Foo {
      | int foo(int param1, int param2) {
      |  return 0;
      | }
      |}
      """.stripMargin

  "should return exactly two parameters with correct fields" in {
    cpg.parameter.name.toSetMutable shouldBe Set("param1", "param2")

    val List(x) = cpg.parameter.filter(_.method.name == "foo").name("param1").l
    x.code shouldBe "int param1"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Some(3)
    x.columnNumber shouldBe Some(10)
    x.order shouldBe 1

    val List(y) = cpg.parameter.filter(_.method.name == "foo").name("param2").l
    y.code shouldBe "int param2"
    y.typeFullName shouldBe "int"
    y.lineNumber shouldBe Some(3)
    y.columnNumber shouldBe Some(22)
    y.order shouldBe 2
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("param1").method.filter(_.isExternal == false).name.l shouldBe List("foo")
  }

}
