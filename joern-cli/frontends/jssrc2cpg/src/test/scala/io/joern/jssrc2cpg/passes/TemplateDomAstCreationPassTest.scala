package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.nodes.TemplateDom
import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language._

class TemplateDomAstCreationPassTest extends AbstractPassTest {

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
      "test.tsx"
    ) { cpg =>
      def parent(c: Expression) = c.parentExpression.get.asInstanceOf[TemplateDom]

      inside(cpg.call.code("formatName.*").l) { case List(call) =>
        parent(call).code shouldBe "{formatName(user)}"
        parent(call).name shouldBe "JSXExpressionContainer"
        parent(parent(call)).code shouldBe "<h1>Hello, {formatName(user)}!</h1>"
        parent(parent(call)).name shouldBe "JSXElement"
      }
    }
  }

}
