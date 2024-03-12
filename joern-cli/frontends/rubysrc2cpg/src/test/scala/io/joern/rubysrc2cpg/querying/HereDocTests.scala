package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Local, Method, Return}
import io.shiftleft.semanticcpg.language.*

class HereDocTests extends RubyCode2CpgFixture(withDataFlow = true) {

  "HereDoc simple" should {
    val cpg = code("""
        | def f()
        |   a = 10
        |   <<-SQL
        |     this is some sql heredoc code
        |   SQL
        |   a
        | end
        |""".stripMargin)

    "have a LITERAL node" in {
      inside(cpg.method.name("f").l) {
        case func :: Nil =>
          inside(func.block.astChildren.l) {
            case (localAst: Local) :: (callAst: Call) :: (literalAst: Literal) :: (returnAst: Return) :: Nil =>
              localAst.code shouldBe "a"
              callAst.code shouldBe "a = 10"

              returnAst.code shouldBe "a"
            case _ =>
          }
        case _ => fail("Expected one method for f")
      }
    }
  }

}
