package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Local}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends RubyCode2CpgFixture {

  "`x ? y : z` is lowered into an if-else expression" in {
    val cpg = code("""x, y, z = false, true, false
                     |x ? y : z
                     |""".stripMargin)

    inside(cpg.controlStructure.l) { case ifStmt :: Nil =>
      ifStmt.code shouldBe "x ? y : z"
      ifStmt.lineNumber shouldBe Some(2)

      inside(ifStmt.condition.l) { case (x: Identifier) :: Nil =>
        x.name shouldBe "x"
        x.lineNumber shouldBe Some(2)
      }

      inside(ifStmt.astChildren.isBlock.l) { case ifBlock :: elseBlock :: Nil =>
        val (y: Identifier) :: Nil = ifBlock.astChildren.l: @unchecked
        y.name shouldBe "y"
        y.lineNumber shouldBe Some(2)

        val (z: Identifier) :: Nil = elseBlock.astChildren.l: @unchecked
        z.name shouldBe "z"
        z.lineNumber shouldBe Some(2)
      }
    }
  }

  "`f(x ? y : z)` is lowered into conditional operator" in {
    val cpg = code("""x, y, z = false, true, false
                     |f(x ? y : z)
                     |""".stripMargin)
    inside(cpg.call(Operators.conditional).l) { case cond :: Nil =>
      inside(cond.argument.l) { case x :: y :: z :: Nil =>
        x.code shouldBe "x"
        List(y, z).isBlock.astChildren.isIdentifier.code.l shouldBe List("y", "z")
      }
    }
  }

  "`f(unless x then y else z end)` is lowered into conditional operator" in {
    val cpg = code("""x, y, z = false, true, false
                     |f(unless x then y else z end)
                     |""".stripMargin)
    inside(cpg.call.nameExact(Operators.conditional).l) { case conditionalCall :: Nil =>
      conditionalCall.code shouldBe "unless x then y else z end"
      inside(conditionalCall.argument.l) { case condition :: (trueBranch: Block) :: (falseBranch: Block) :: Nil =>
        condition.code shouldBe "x"

        val List(trueBranchIdent) = trueBranch.astChildren.isIdentifier.l
        trueBranchIdent.code shouldBe "z"

        val List(falseBranchIdent) = falseBranch.astChildren.isIdentifier.l
        falseBranchIdent.code shouldBe "y"

      }
    }
  }

}
