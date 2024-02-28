package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local}
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends RubyCode2CpgFixture {

  "`x ? y : z` is lowered into an if-else expression" in {
    val cpg = code("""
                     |x ? y : z
                     |""".stripMargin)

    inside(cpg.controlStructure.l) {
      case ifStmt :: Nil =>
        ifStmt.code shouldBe "x ? y : z"
        ifStmt.lineNumber shouldBe Some(2)

        inside(ifStmt.condition.l) {
          case (x: Identifier) :: Nil =>
            x.name shouldBe "x"
            x.lineNumber shouldBe Some(2)
          case xs => fail(s"Expected exactly one identifier conditional, got [${xs.code.mkString(",")}]")
        }

        inside(ifStmt.astChildren.isBlock.l) {
          case ifBlock :: elseBlock :: Nil =>
            val (_: Local) :: (y: Identifier) :: Nil = ifBlock.astChildren.l: @unchecked
            y.name shouldBe "y"
            y.lineNumber shouldBe Some(2)

            val (_: Local) :: (z: Identifier) :: Nil = elseBlock.astChildren.l: @unchecked
            z.name shouldBe "z"
            z.lineNumber shouldBe Some(2)
          case xs => fail(s"Expected exactly two blocks under the if-structure, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected exactly one control structure, got [${xs.code.mkString(",")}]")
    }
  }

}
