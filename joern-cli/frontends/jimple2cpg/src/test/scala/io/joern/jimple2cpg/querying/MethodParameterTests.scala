package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class MethodParameterTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """package a;
      |class Foo {
      | int foo(int param1, int param2) {
      |  return 0;
      | }
      |}
      """.stripMargin

  "should return exactly two parameters with correct fields" in {
    cpg.parameter.filter(_.method.name == "foo").name.toSet shouldBe Set("param1", "param2")

    val List(x) = cpg.parameter.filter(_.method.name == "foo").name("param1").l
    x.code shouldBe "int param1"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Some(3)
    x.columnNumber shouldBe Some(-1)
    x.order shouldBe 1

    val List(y) = cpg.parameter.filter(_.method.name == "foo").name("param2").l
    y.code shouldBe "int param2"
    y.typeFullName shouldBe "int"
    y.lineNumber shouldBe Some(3)
    y.columnNumber shouldBe Some(-1)
    y.order shouldBe 2
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("param1").method.filter(_.isExternal == false).name.l shouldBe List("foo")
  }

}
