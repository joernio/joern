package io.joern.jssrc2cpg.passes.ast

import io.joern.jssrc2cpg.passes.AbstractDomPassTest
import io.shiftleft.semanticcpg.language._

class TemplateDomAstCreationPassTest extends AbstractDomPassTest {

  "AST generation for template DOM" should {

    "have correct structure for simple JSX" in AstFixture(
      """
        |const element = (
        |  <div>
        |    <h1>Hello!</h1>
        |    <h2>Good to see you here.</h2>
        |  </div>
        |);
        |
        |function getGreeting(user) {
        |  if (user) {
        |    return <h1>Hello, {formatName(user)}!</h1>;
        |  }
        |  return <h1>Hello, Stranger.</h1>;
        |}
        |""".stripMargin,
      "code.tsx"
    ) { cpg =>
      inside(cpg.call.code("formatName.*").l) { case List(call) =>
        parentTemplateDom(call).code shouldBe "{formatName(user)}"
        parentTemplateDom(call).name shouldBe "JSXExpressionContainer"
        parentTemplateDom(parentTemplateDom(call)).code shouldBe "<h1>Hello, {formatName(user)}!</h1>"
        parentTemplateDom(parentTemplateDom(call)).name shouldBe "JSXElement"
      }
    }
  }

}
