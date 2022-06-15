package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TemplateDom
import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside

class TemplateDomAstCreationPassTest extends AbstractPassTest with Inside {

  "AST generation for template DOM" should {

    "have correct structure for simple JSX" in AstFixture("""
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
        |""".stripMargin) { cpg =>
      def parent(c: Expression) = c.parentExpression.get.asInstanceOf[TemplateDom]

      inside(cpg.call.code("formatName.*").l) { case List(call) =>
        parent(call).code shouldBe "{formatName(user)}"
        parent(call).name shouldBe "JSXExpressionContainer"
        parent(parent(call)).code shouldBe "<h1>Hello, {formatName(user)}!</h1>"
        parent(parent(call)).name shouldBe "JSXElement"
      }
    }
  }

  private object AstFixture extends Fixture {
    def apply(code: String)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val file = dir / "code.tsx"
        file.write(code)
        file.deleteOnExit()
        val cpg = new JsSrc2CpgFrontend().execute(dir.toJava)
        f(cpg)
      }
    }
  }

}
