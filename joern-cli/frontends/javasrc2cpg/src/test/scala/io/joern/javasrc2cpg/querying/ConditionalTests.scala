package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends JavaSrcCode2CpgFixture {

  "should parse ternary expression" in {
    lazy val cpg = code("""
        |class Foo {
        |  public int foo(int x) {
        |    int y = (x > 5) ? 10 : 2 + 20;
        |    return y;
        |  }
        |}
        |""".stripMargin)

    val List(ternaryExpr: Call)                                  = cpg.method.name("foo").call(Operators.conditional).l
    val List(condition: Call, thenExpr: Literal, elseExpr: Call) = ternaryExpr.argument.l: @unchecked

    condition.code shouldBe "x > 5"
    condition.methodFullName shouldBe Operators.greaterThan

    thenExpr.code shouldBe "10"
    thenExpr.typeFullName shouldBe "int"

    elseExpr.code shouldBe "2 + 20"
    elseExpr.methodFullName shouldBe Operators.addition
  }

  "should find unresolved field-access args" in {
    lazy val cpg = code("""
        |class Foo {
        |  public int[] bar(boolean allowNull) {
        |    int[] y = allowNull ? this.cache : this.cacheNoNull;
        |    return y;
        |  }
        |}
        |""".stripMargin)
    val List(conditional: Call) = cpg.method.name("bar").call(Operators.conditional).l
    val List(condition: Identifier, thenExpr: Call, elseExpr: Call) = conditional.argument.l: @unchecked

    condition.code shouldBe "allowNull"
    condition.typeFullName shouldBe "boolean"

    thenExpr.code shouldBe "this.cache"
    thenExpr.name shouldBe Operators.fieldAccess

    elseExpr.code shouldBe "this.cacheNoNull"
    elseExpr.name shouldBe Operators.fieldAccess
  }

  "should be able to parse nested conditionals" in {
    lazy val cpg = code("""
      |class Foo {
      |  public int baz(int input) {
      |    return (input > 10) ? 55 : ((input < 15) ? 42 : 39);
      |  }
      |}
      |""".stripMargin)
    val method = cpg.method.name("baz").head
    method.call(Operators.conditional).size shouldBe 2
    val List(parentC: Call, childC: Call) = method.call(Operators.conditional).l

    parentC.argument.size shouldBe 3
    childC.argument.size shouldBe 3
  }
}
