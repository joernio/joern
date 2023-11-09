package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SimpleAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple fragments" should {

    "have correct structure for simple variable decl" in AstFixture("""
        |let x = 1;
        |""".stripMargin) { cpg =>
      val List(method) = cpg.method.nameExact("<global>").l
      method.astChildren.code.l shouldBe List("let x = 1;")
    }
  }

}
