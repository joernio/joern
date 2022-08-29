package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

class MethodParameterTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """
      |contract Foo {
      | function foo(int param1, uint[] memory param2) public returns (uint8) {
      |  return 0;
      | }
      |}
      """.stripMargin

  "should return exactly three parameters with correct fields" in {
    cpg.parameter.filter(_.method.name == "foo").name.toSetMutable shouldBe Set("this", "param1", "param2")

    // the concept of "this" is implicitly passed to non-static functions
    val List(t) = cpg.parameter.filter(_.method.name == "foo").name("this").l
    t.code shouldBe "this"
    t.typeFullName shouldBe "Foo"
//    t.lineNumber shouldBe Some(3)
//    t.columnNumber shouldBe None
    t.order shouldBe 0
    t.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING

    val List(x) = cpg.parameter.filter(_.method.name == "foo").name("param1").l
    x.code shouldBe "int param1"
    x.typeFullName shouldBe "int"
//    x.lineNumber shouldBe Some(3)
//    x.columnNumber shouldBe Some(15)
    x.order shouldBe 1
    x.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE

    val List(y) = cpg.parameter.filter(_.method.name == "foo").name("param2").l
    y.code shouldBe "uint[] memory param2"
    y.typeFullName shouldBe "uint[] memory"
//    y.lineNumber shouldBe Some(3)
//    y.columnNumber shouldBe Some(27)
    y.order shouldBe 2
    y.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
  }

  "should allow traversing from parameter to method" in {
    cpg.parameter.name("param1").method.filter(_.isExternal == false).name.l shouldBe List("foo")
  }

}
