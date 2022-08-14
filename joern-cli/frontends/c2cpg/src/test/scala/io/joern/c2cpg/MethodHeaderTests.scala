package io.joern.c2cpg

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language._

class MethodHeaderTests extends CCodeToCpgSuite {

  "Method header" should {
    val cpg = code("""
        |int foo(int x, int y) {
        |
        |}
        |""".stripMargin)

    "have correct METHOD node for method foo" in {
      val List(method) = cpg.method.nameExact("foo").l
      method.isExternal shouldBe false
      method.fullName shouldBe "foo"
      method.signature shouldBe "int foo (int,int)"
      method.lineNumber shouldBe Some(2)
      method.columnNumber shouldBe Some(1)
      method.lineNumberEnd shouldBe Some(4)
      method.columnNumberEnd shouldBe Some(1)
      method.code shouldBe "int foo (int x,int y)"
    }

    "have correct METHOD_PARAMETER_IN nodes for method foo" in {
      val List(param1, param2) = cpg.method.nameExact("foo").parameter.l
      param1.order shouldBe 1
      param1.code shouldBe "int x"
      param1.name shouldBe "x"
      param1.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      param1.lineNumber shouldBe Some(2)
      param1.columnNumber shouldBe Some(9)

      param2.order shouldBe 2
      param2.code shouldBe "int y"
      param2.name shouldBe "y"
      param2.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      param2.lineNumber shouldBe Some(2)
      param2.columnNumber shouldBe Some(16)
    }

    "have correct METHOD_RETURN node for method foo" in {
      val List(ret) = cpg.method.nameExact("foo").methodReturn.l
      ret.code shouldBe "int"
      ret.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      ret.lineNumber shouldBe Some(2)
      ret.columnNumber shouldBe Some(1)
    }

  }

}
