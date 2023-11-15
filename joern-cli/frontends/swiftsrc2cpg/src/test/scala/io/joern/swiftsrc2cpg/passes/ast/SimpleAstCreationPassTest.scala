package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SimpleAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple fragments" should {

    "have correct structure for simple variable decl" in AstFixture("""
        |let x = 1
        |""".stripMargin) { cpg =>
      val List(method) = cpg.method.nameExact("<global>").l
      method.astChildren.isBlock.astChildren.code.l shouldBe List("let x = 1")
    }

    "have correct structure for annotated function" in AstFixture("""
        |@bar(x: "y")
        |func foo() -> {
        |  let x = 1
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.nameExact("foo").annotation.l) { case List(bar) =>
        bar.code shouldBe """@bar(x: "y")"""
        bar.name shouldBe "bar"
        bar.fullName shouldBe "bar"
        val List(paramAssignFoo) = bar.parameterAssign.l
        paramAssignFoo.code shouldBe """x: "y""""
        paramAssignFoo.order shouldBe 1
        val List(paramFoo) = paramAssignFoo.parameter.l
        paramFoo.code shouldBe "argument"
        paramFoo.order shouldBe 1
        val List(paramValueFoo) = paramAssignFoo.value.l
        paramValueFoo.code shouldBe """x: "y""""
        paramValueFoo.order shouldBe 2
        paramValueFoo.argumentIndex shouldBe 2
      }
    }
  }

}
