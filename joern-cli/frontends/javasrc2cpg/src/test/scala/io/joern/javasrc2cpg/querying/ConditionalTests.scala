package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal}
import io.shiftleft.semanticcpg.language._

class ConditionalTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |class Foo {
      |  public int foo(int x) {
      |    int y = (x > 5) ? 10 : 2 + 20;
      |    return y;
      |  }
      |}
      |""".stripMargin

  "should parse ternary expression" in {
    val List(ternaryExpr: Call) = cpg.call(Operators.conditional).l
    val List(condition: Call, thenExpr: Literal, elseExpr: Call) = ternaryExpr.argument.l

    condition.code shouldBe "x > 5"
    condition.methodFullName shouldBe Operators.greaterThan

    thenExpr.code shouldBe "10"
    thenExpr.typeFullName shouldBe "int"

    elseExpr.code shouldBe "2 + 20"
    elseExpr.methodFullName shouldBe Operators.addition
  }
}