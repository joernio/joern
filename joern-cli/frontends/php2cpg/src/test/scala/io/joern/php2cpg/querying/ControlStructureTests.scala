package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  ControlStructure,
  Identifier,
  JumpLabel,
  JumpTarget,
  Literal
}
import io.shiftleft.semanticcpg.language._

class ControlStructureTests extends PhpCode2CpgFixture {
  "switch statements" should {
    "work without a default case" in {
      val cpg = code("""<?php
			 |switch ($cond) {
			 |  case 0:
			 |    $b;
			 |    break;
			 |  case 1:
			 |    $c;
			 |    break;
			 |}
			 |""".stripMargin)

      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l) { case List(switchStmt) =>
        switchStmt.lineNumber shouldBe Some(2)

        inside(switchStmt.condition.l) { case List(cond: Identifier) =>
          cond.name shouldBe "cond"
          cond.code shouldBe "$cond"
          cond.lineNumber shouldBe Some(2)
        }

        inside(switchStmt.astChildren.l) {
          case List(
                _,
                case0: JumpTarget,
                bIdent: Identifier,
                break1: ControlStructure,
                case1: JumpTarget,
                cIdent: Identifier,
                break2: ControlStructure
              ) =>
            case0.name shouldBe "case"
            case0.code shouldBe "case 0"
            case0.lineNumber shouldBe Some(3)

            bIdent.name shouldBe "b"
            bIdent.code shouldBe "$b"
            bIdent.lineNumber shouldBe Some(4)

            break1.controlStructureType shouldBe ControlStructureTypes.BREAK
            break1.code shouldBe "break"
            break1.lineNumber shouldBe Some(5)

            case1.name shouldBe "case"
            case1.code shouldBe "case 1"
            case1.lineNumber shouldBe Some(6)

            cIdent.name shouldBe "c"
            cIdent.code shouldBe "$c"
            cIdent.lineNumber shouldBe Some(7)

            break2.controlStructureType shouldBe ControlStructureTypes.BREAK
            break2.code shouldBe "break"
            break2.lineNumber shouldBe Some(8)
        }
      }
    }

    "work with a default case" in {
      val cpg = code("""<?php
                      |switch ($cond) {
                      |  case 0:
                      |    $b;
                      |    break;
                      |  default:
                      |    $c;
                      |}
                      |""".stripMargin)

      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l) { case List(switchStmt) =>
        switchStmt.lineNumber shouldBe Some(2)

        inside(switchStmt.condition.l) { case List(cond: Identifier) =>
          cond.name shouldBe "cond"
          cond.code shouldBe "$cond"
          cond.lineNumber shouldBe Some(2)
        }

        inside(switchStmt.astChildren.l) {
          case List(
                _,
                case0: JumpTarget,
                bIdent: Identifier,
                break1: ControlStructure,
                defaultCase: JumpTarget,
                cIdent: Identifier
              ) =>
            case0.name shouldBe "case"
            case0.code shouldBe "case 0"
            case0.lineNumber shouldBe Some(3)

            bIdent.name shouldBe "b"
            bIdent.code shouldBe "$b"
            bIdent.lineNumber shouldBe Some(4)

            break1.controlStructureType shouldBe ControlStructureTypes.BREAK
            break1.code shouldBe "break"
            break1.lineNumber shouldBe Some(5)

            defaultCase.name shouldBe "default"
            defaultCase.code shouldBe "default"
            defaultCase.lineNumber shouldBe Some(6)

            cIdent.name shouldBe "c"
            cIdent.lineNumber shouldBe Some(7)
        }
      }
    }
  }

  "if statements" should {
    "work without a body, an elseif or an else" in {
      val cpg = code("<?php\nif ($a) {}")

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block) =>
        thenBlock.astChildren.size shouldBe 0
      }
    }

    "work with just a then body" in {
      val cpg = code("<?php\nif ($a) { $b; }")

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(bIdentifier: Identifier) =>
          bIdentifier.name shouldBe "b"
        }
      }
    }

    "work with else" in {
      val cpg = code("<?php\nif ($a) { $b; } else { $c; }")

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(bIdentifier: Identifier) =>
          bIdentifier.name shouldBe "b"
        }

        inside(elseBlock.astChildren.l) { case List(cIdentifier: Identifier) =>
          cIdentifier.name shouldBe "c"
        }
      }
    }

    "work with elseif chains" in {
      val cpg = code("""<?php
                      |if ($cond1) {
                      |  $body1;
                      |} elseif ($cond2) {
                      |  $body2;
                      |} elseif ($cond3) {
                      |  $body3;
                      |} else {
                      |  $body4;
                      |}
                      |""".stripMargin)

      val ifAst = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).head

      inside(ifAst.condition.l) { case List(condition1: Identifier) =>
        condition1.name shouldBe "cond1"
      }

      val elseif1 = inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body1: Identifier) =>
          body1.name shouldBe "body1"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif1.condition.l) { case List(condition2: Identifier) =>
        condition2.name shouldBe "cond2"
      }

      val elseif2 = inside(elseif1.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body2: Identifier) =>
          body2.name shouldBe "body2"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif2.condition.l) { case List(condition3: Identifier) =>
        condition3.name shouldBe "cond3"
      }

      inside(elseif2.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body3: Identifier) =>
          body3.name shouldBe "body3"
        }

        inside(elseBlock.astChildren.l) { case List(body4: Identifier) =>
          body4.name shouldBe "body4"
        }
      }
    }

    "work with else...if chains" in {
      val cpg = code("""<?php
                      |if ($cond1) {
                      |  $body1;
                      |} else if ($cond2) {
                      |  $body2;
                      |} else if ($cond3) {
                      |  $body3;
                      |} else {
                      |  $body4;
                      |}
                      |""".stripMargin)

      val ifAst = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).head

      inside(ifAst.condition.l) { case List(condition1: Identifier) =>
        condition1.name shouldBe "cond1"
      }

      val elseif1 = inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body1: Identifier) =>
          body1.name shouldBe "body1"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif1.condition.l) { case List(condition2: Identifier) =>
        condition2.name shouldBe "cond2"
      }

      val elseif2 = inside(elseif1.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body2: Identifier) =>
          body2.name shouldBe "body2"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif2.condition.l) { case List(condition3: Identifier) =>
        condition3.name shouldBe "cond3"
      }

      inside(elseif2.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body3: Identifier) =>
          body3.name shouldBe "body3"
        }

        inside(elseBlock.astChildren.l) { case List(body4: Identifier) =>
          body4.name shouldBe "body4"
        }
      }
    }

    "work with elseif chains with colon syntax" in {
      val cpg = code("""<?php
                      |if ($cond1):
                      |  $body1;
                      |elseif ($cond2):
                      |  $body2;
                      |elseif ($cond3):
                      |  $body3;
                      |else:
                      |  $body4;
                      |endif;
                      |""".stripMargin)

      val ifAst = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).head

      inside(ifAst.condition.l) { case List(condition1: Identifier) =>
        condition1.name shouldBe "cond1"
      }

      val elseif1 = inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body1: Identifier) =>
          body1.name shouldBe "body1"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif1.condition.l) { case List(condition2: Identifier) =>
        condition2.name shouldBe "cond2"
      }

      val elseif2 = inside(elseif1.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body2: Identifier) =>
          body2.name shouldBe "body2"
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      inside(elseif2.condition.l) { case List(condition3: Identifier) =>
        condition3.name shouldBe "cond3"
      }

      inside(elseif2.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body3: Identifier) =>
          body3.name shouldBe "body3"
        }

        inside(elseBlock.astChildren.l) { case List(body4: Identifier) =>
          body4.name shouldBe "body4"
        }
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

      whileAst.code shouldBe "while($a)"

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

      whileAst.code shouldBe "while($a)"

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
