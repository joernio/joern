package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language._

class MethodParameterTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """package a;
      |class Foo {
      | int foo(int param1, Object param2) {
      |  return 0;
      | }
      |}
      """.stripMargin

  "should return exactly three parameters with correct fields" in {
    cpg.parameter.filter(_.method.name == "foo").name.toSetMutable shouldBe Set("this", "param1", "param2")

    val List(t) = cpg.parameter.filter(_.method.name == "foo").name("this").l
    t.code shouldBe "this"
    t.typeFullName shouldBe "a.Foo"
    t.lineNumber shouldBe Some(3)
    t.columnNumber shouldBe None
    t.order shouldBe 0
    t.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING

    val List(x) = cpg.parameter.filter(_.method.name == "foo").name("param1").l
    x.code shouldBe "int param1"
    x.typeFullName shouldBe "int"
    x.lineNumber shouldBe Some(3)
    x.columnNumber shouldBe None
    x.order shouldBe 1
    x.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

    val List(y) = cpg.parameter.filter(_.method.name == "foo").name("param2").l
    y.code shouldBe "java.lang.Object param2"
    y.typeFullName shouldBe "java.lang.Object"
    y.lineNumber shouldBe Some(3)
    y.columnNumber shouldBe None
    y.order shouldBe 2
    y.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("param1").method.filter(_.isExternal == false).name.l shouldBe List("foo")
  }

}
