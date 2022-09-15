package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Identifier, Literal}
import io.shiftleft.semanticcpg.language._

class ControlStructureTests extends PhpCode2CpgFixture {

  "if statements" should {
    "work without a body, an elseif or an else" in {
      val cpg = code("<?php\nif ($a) {}")

      val ifAst = cpg.controlStructure.l match {
        case List(ast) =>
          ast.controlStructureType shouldBe ControlStructureTypes.IF
          ast

        case result => fail(s"Expected single control structure but found $result")
      }

      ifAst.condition.l match {
        case List(aIdent: Identifier) =>
          aIdent.name shouldBe "a"

        case result => fail(s"Expected identifier argument but found $result")
      }

      ifAst.astChildren.l match {
        case List(_, thenBlock: Block) =>
          thenBlock.astChildren.size shouldBe 0
        case result => fail(s"Expected only then body but found $result")
      }
    }
  }

  "break statements" should {
    "support the default depth 1 break" in {
      val cpg = code("<?php\nbreak;")
      cpg.controlStructure.l match {
        case List(breakStmt) =>
          breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK
          breakStmt.astChildren.isEmpty shouldBe true

        case result => fail(s"Expected break but found $result")
      }
    }

    "support arbitrary depth breaks" in {
      val cpg = code("<?php\nbreak(5);")
      cpg.controlStructure.l match {
        case List(breakStmt) =>
          breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK
          breakStmt.astChildren.l match {
            case List(num: Literal) =>
              num.code shouldBe "5"
              num.typeFullName shouldBe TypeConstants.Int

            case result => fail(s"Expected depth argument but found $result")
          }

        case result => fail(s"Expected break but found $result")
      }
    }
  }

  "continue statements" should {
    "support the default depth 1 continue" in {
      val cpg = code("<?php\ncontinue;")
      cpg.controlStructure.l match {
        case List(breakStmt) =>
          breakStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE
          breakStmt.astChildren.isEmpty shouldBe true

        case result => fail(s"Expected continue but found $result")
      }
    }

    "support arbitrary depth continues" in {
      val cpg = code("<?php\ncontinue(5);")
      cpg.controlStructure.l match {
        case List(breakStmt) =>
          breakStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE
          breakStmt.astChildren.l match {
            case List(num: Literal) =>
              num.code shouldBe "5"
              num.typeFullName shouldBe TypeConstants.Int

            case result => fail(s"Expected depth argument but found $result")
          }

        case result => fail(s"Expected continue but found $result")
      }
    }
  }

  "while statements" should {
    "work with an empty body" in {
      val cpg = code("<?php\nwhile($a);")
      val whileAst = cpg.controlStructure.l match {
        case List(whileAst) if whileAst.controlStructureType == ControlStructureTypes.WHILE => whileAst
        case result => fail(s"Expected while but found $result")
      }

      whileAst.code shouldBe "while(a)"

      whileAst.condition.l match {
        case List(aIdent: Identifier) =>
          aIdent.name shouldBe "a"
          aIdent.order shouldBe 1
        case result => fail(s"Expected while condition but found $result")
      }

      whileAst.astChildren.collectAll[Block].l match {
        case List(block) =>
          block.order shouldBe 2
          block.astChildren.size shouldBe 0

        case result => fail(s"Expected while block but found $result")
      }
    }

    "work with a non-empty body" in {
      val cpg = code("""<?php
				 |while ($a) {
				 |  $b;
				 |  $c;
				 |}""".stripMargin)
      val whileAst = cpg.controlStructure.l match {
        case List(whileAst) if whileAst.controlStructureType == ControlStructureTypes.WHILE => whileAst
        case result => fail(s"Expected while but found $result")
      }

      whileAst.code shouldBe "while(a)"

      whileAst.condition.l match {
        case List(aIdent: Identifier) =>
          aIdent.name shouldBe "a"
        case result => fail(s"Expected while condition but found $result")
      }

      whileAst.astChildren.collectAll[Block].l match {
        case List(block) =>
          block.astChildren.l match {
            case List(bIdent: Identifier, cIdent: Identifier) =>
              bIdent.name shouldBe "b"
              cIdent.name shouldBe "c"
            case result => fail(s"Expected while body statements but got $result")
          }

        case result => fail(s"Expected while block but found $result")
      }
    }
  }
}
