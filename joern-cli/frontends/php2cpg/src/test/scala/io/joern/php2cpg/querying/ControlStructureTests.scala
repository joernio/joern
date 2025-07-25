package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  Block,
  Call,
  ControlStructure,
  Identifier,
  JumpLabel,
  JumpTarget,
  Literal,
  Local
}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import scala.util.Try

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
       |};
       |""".stripMargin)

      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l) { case List(switchStmt) =>
        switchStmt.code shouldBe "switch ($cond)"
        switchStmt.lineNumber shouldBe Some(2)

        inside(switchStmt.condition.l) { case List(cond: Identifier) =>
          cond.name shouldBe "cond"
          cond.code shouldBe "$cond"
          cond.lineNumber shouldBe Some(2)
        }

        inside(switchStmt.whenTrue.astChildren.l) {
          case List(
                case0: JumpTarget,
                cond0: Literal,
                bIdent: Identifier,
                break1: ControlStructure,
                case1: JumpTarget,
                cond1: Literal,
                cIdent: Identifier,
                break2: ControlStructure
              ) =>
            case0.name shouldBe "case"
            case0.code shouldBe "case 0"
            case0.lineNumber shouldBe Some(3)

            cond0.code shouldBe "0"
            cond0.lineNumber shouldBe Some(3)

            bIdent.name shouldBe "b"
            bIdent.code shouldBe "$b"
            bIdent.lineNumber shouldBe Some(4)

            break1.controlStructureType shouldBe ControlStructureTypes.BREAK
            break1.code shouldBe "break"
            break1.lineNumber shouldBe Some(5)

            case1.name shouldBe "case"
            case1.code shouldBe "case 1"
            case1.lineNumber shouldBe Some(6)

            cond1.code shouldBe "1"
            cond1.lineNumber shouldBe Some(6)

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
        |};
        |""".stripMargin)

      inside(cpg.controlStructure.controlStructureType(ControlStructureTypes.SWITCH).l) { case List(switchStmt) =>
        switchStmt.code shouldBe "switch ($cond)"
        switchStmt.lineNumber shouldBe Some(2)

        inside(switchStmt.condition.l) { case List(cond: Identifier) =>
          cond.name shouldBe "cond"
          cond.code shouldBe "$cond"
          cond.lineNumber shouldBe Some(2)
        }

        inside(switchStmt.whenTrue.astChildren.l) {
          case List(
                case0: JumpTarget,
                cond0: Literal,
                bIdent: Identifier,
                break1: ControlStructure,
                defaultCase: JumpTarget,
                cIdent: Identifier
              ) =>
            case0.name shouldBe "case"
            case0.code shouldBe "case 0"
            case0.lineNumber shouldBe Some(3)

            cond0.code shouldBe "0"
            cond0.lineNumber shouldBe Some(3)

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
            cIdent.code shouldBe "$c"
            cIdent.lineNumber shouldBe Some(7)
        }
      }
    }
  }

  "if statements" should {
    "work without a body, an elseif or an else" in {
      val cpg = code("""<?php
        |if ($a) {};
        |""".stripMargin)

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      ifAst.code shouldBe "if ($a)"
      ifAst.lineNumber shouldBe Some(2)

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.code shouldBe "$a"
        aIdent.lineNumber shouldBe Some(2)
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block) =>
        thenBlock.astChildren.size shouldBe 0
        thenBlock.lineNumber shouldBe Some(2)
      }
    }

    "work with just a then body" in {
      val cpg = code("""<?php
        |if ($a) { $b; };
        |""".stripMargin)

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      ifAst.code shouldBe "if ($a)"
      ifAst.lineNumber shouldBe Some(2)

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.code shouldBe "$a"
        aIdent.lineNumber shouldBe Some(2)
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block) =>
        thenBlock.lineNumber shouldBe Some(2)

        inside(thenBlock.astChildren.l) { case List(bIdentifier: Identifier) =>
          bIdentifier.name shouldBe "b"
          bIdentifier.code shouldBe "$b"
          bIdentifier.lineNumber shouldBe Some(2)
        }
      }
    }

    "work with else" in {
      val cpg = code("""<?php
        |if ($a) { $b; } else { $c; };
        |""".stripMargin)

      val ifAst = inside(cpg.controlStructure.l) { case List(ast) =>
        ast.controlStructureType shouldBe ControlStructureTypes.IF
        ast
      }

      ifAst.code shouldBe "if ($a)"
      ifAst.lineNumber shouldBe Some(2)

      inside(ifAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.code shouldBe "$a"
        aIdent.lineNumber shouldBe Some(2)
      }

      inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        thenBlock.lineNumber shouldBe Some(2)
        elseBlock.lineNumber shouldBe Some(2)

        inside(thenBlock.astChildren.l) { case List(bIdentifier: Identifier) =>
          bIdentifier.name shouldBe "b"
          bIdentifier.code shouldBe "$b"
          bIdentifier.lineNumber shouldBe Some(2)
        }

        inside(elseBlock.astChildren.l) { case List(cIdentifier: Identifier) =>
          cIdentifier.name shouldBe "c"
          cIdentifier.code shouldBe "$c"
          cIdentifier.lineNumber shouldBe Some(2)
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
        |};
        |""".stripMargin)

      val ifAst = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).head

      inside(ifAst.condition.l) { case List(condition1: Identifier) =>
        condition1.name shouldBe "cond1"
        condition1.code shouldBe "$cond1"
        condition1.lineNumber shouldBe Some(2)
      }

      val elseif1 = inside(ifAst.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body1: Identifier) =>
          body1.name shouldBe "body1"
          body1.code shouldBe "$body1"
          body1.lineNumber shouldBe Some(3)
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      elseif1.lineNumber shouldBe Some(4)

      inside(elseif1.condition.l) { case List(condition2: Identifier) =>
        condition2.name shouldBe "cond2"
        condition2.code shouldBe "$cond2"
        condition2.lineNumber shouldBe Some(4)
      }

      val elseif2 = inside(elseif1.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        inside(thenBlock.astChildren.l) { case List(body2: Identifier) =>
          body2.name shouldBe "body2"
          body2.code shouldBe "$body2"
          body2.lineNumber shouldBe Some(5)
        }

        inside(elseBlock.astChildren.l) { case List(elseStructure: ControlStructure) =>
          elseStructure
        }
      }

      elseif2.lineNumber shouldBe Some(6)

      inside(elseif2.condition.l) { case List(condition3: Identifier) =>
        condition3.name shouldBe "cond3"
        condition3.code shouldBe "$cond3"
        condition3.lineNumber shouldBe Some(6)
      }

      inside(elseif2.astChildren.l) { case List(_, thenBlock: Block, elseBlock: Block) =>
        thenBlock.lineNumber shouldBe Some(6)
        elseBlock.lineNumber shouldBe Some(8)

        inside(thenBlock.astChildren.l) { case List(body3: Identifier) =>
          body3.name shouldBe "body3"
          body3.code shouldBe "$body3"
          body3.lineNumber shouldBe Some(7)
        }

        inside(elseBlock.astChildren.l) { case List(body4: Identifier) =>
          body4.name shouldBe "body4"
          body4.code shouldBe "$body4"
          body4.lineNumber shouldBe Some(9)
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
        |};
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
      val cpg = code("""<?php
        |break;
        |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(breakStmt) =>
        breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK
        breakStmt.astChildren.isEmpty shouldBe true
      }
    }

    "support arbitrary depth breaks" in {
      val cpg = code("""<?php
        |break(5);
        |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(breakStmt) =>
        breakStmt.controlStructureType shouldBe ControlStructureTypes.BREAK

        inside(breakStmt.astChildren.l) { case List(num: Literal) =>
          num.code shouldBe "5"
          num.typeFullName shouldBe TypeConstants.Int
        }
      }
    }
  }

  "continue statements" should {
    "support the default depth 1 continue" in {
      val cpg = code("""<?php
        |continue;
        |""".stripMargin)
      inside(cpg.controlStructure.l) { case List(continueStmt) =>
        continueStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE
        continueStmt.astChildren.isEmpty shouldBe true
        continueStmt.lineNumber shouldBe Some(2)
      }
    }

    "support arbitrary depth continues" in {
      val cpg = code("""<?php
        |continue(5);
        |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(continueStmt) =>
        continueStmt.controlStructureType shouldBe ControlStructureTypes.CONTINUE

        inside(continueStmt.astChildren.l) { case List(num: Literal) =>
          num.code shouldBe "5"
          num.typeFullName shouldBe TypeConstants.Int
        }
      }
    }
  }

  "while statements" should {
    "work with an empty body" in {
      val cpg = code("""<?php
        |while($a);
        |""".stripMargin)
      val whileAst = inside(cpg.controlStructure.l) {
        case List(whileAst) if whileAst.controlStructureType == ControlStructureTypes.WHILE => whileAst
      }

      whileAst.code shouldBe "while ($a)"

      inside(whileAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.order shouldBe 1
      }

      inside(whileAst.astChildren.collectAll[Block].l) { case List(block) =>
        block.order shouldBe 2
        block.astChildren.size shouldBe 0
      }
    }

    "work with a non-empty body" in {
      val cpg = code("""<?php
       |while ($a) {
       |  $b;
       |  $c;
       |};
       |""".stripMargin)

      val whileAst = inside(cpg.controlStructure.l) {
        case List(whileAst) if whileAst.controlStructureType == ControlStructureTypes.WHILE => whileAst
      }

      whileAst.code shouldBe "while ($a)"
      whileAst.lineNumber shouldBe Some(2)

      inside(whileAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.code shouldBe "$a"
        aIdent.lineNumber shouldBe Some(2)
      }

      inside(whileAst.astChildren.collectAll[Block].l) { case List(block) =>
        block.lineNumber shouldBe Some(2)

        inside(block.astChildren.l) { case List(bIdent: Identifier, cIdent: Identifier) =>
          bIdent.name shouldBe "b"
          bIdent.code shouldBe "$b"
          bIdent.lineNumber shouldBe Some(3)

          cIdent.name shouldBe "c"
          cIdent.code shouldBe "$c"
          cIdent.lineNumber shouldBe Some(4)
        }
      }
    }
  }

  "do statements" should {
    "work with an empty body" in {
      val cpg = code("""<?php
        |do {} while ($a);
        |""".stripMargin)
      val doASt = inside(cpg.controlStructure.l) {
        case List(doAst) if doAst.controlStructureType == ControlStructureTypes.DO => doAst
      }

      doASt.code shouldBe "do {...} while ($a)"

      inside(doASt.astChildren.collectAll[Block].l) { case List(block) =>
        block.order shouldBe 1
        block.astChildren.size shouldBe 0
      }

      inside(doASt.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.order shouldBe 2
      }
    }

    "work with a non-empty body" in {
      val cpg = code("""<?php
       |do {
       |  $b;
       |  $c;
       |} while ($a);""".stripMargin)

      val doAst = inside(cpg.controlStructure.l) {
        case List(doAst) if doAst.controlStructureType == ControlStructureTypes.DO => doAst
      }

      doAst.code shouldBe "do {...} while ($a)"
      doAst.lineNumber shouldBe Some(2)

      inside(doAst.astChildren.collectAll[Block].l) { case List(block) =>
        block.lineNumber shouldBe Some(2)

        inside(block.astChildren.l) { case List(bIdent: Identifier, cIdent: Identifier) =>
          bIdent.name shouldBe "b"
          bIdent.code shouldBe "$b"
          bIdent.lineNumber shouldBe Some(3)

          cIdent.name shouldBe "c"
          cIdent.code shouldBe "$c"
          cIdent.lineNumber shouldBe Some(4)
        }
      }

      inside(doAst.condition.l) { case List(aIdent: Identifier) =>
        aIdent.name shouldBe "a"
        aIdent.code shouldBe "$a"
        aIdent.lineNumber shouldBe Some(5)
      }
    }
  }

  "for statements with the usual format" should {
    val cpg = code("""<?php
      |for ($i = 0; $i < 42; $i++) {
      |  echo $i;
      |};
      |""".stripMargin)

    "add a local for the initializer to the enclosing method" in {
      inside(cpg.local.l) { case List(iLocal) =>
        iLocal.name shouldBe "i"
        iLocal.code shouldBe "$i"
      }
    }

    "create the FOR control structure" in {
      inside(cpg.controlStructure.l) { case List(forStructure) =>
        forStructure.controlStructureType shouldBe ControlStructureTypes.FOR
        forStructure.code shouldBe "for ($i = 0;$i < 42;$i++)"
        forStructure.lineNumber shouldBe Some(2)
      }
    }

    "create the correct initialiser AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(initialiser, _, _, _) =>
        initialiser.code shouldBe "$i = 0"
        initialiser.lineNumber shouldBe Some(2)
      }
    }

    "create the correct condition AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, condition, _, _) =>
        condition.code shouldBe "$i < 42"
        condition.lineNumber shouldBe Some(2)

        cpg.controlStructure.condition.l shouldBe List(condition)
      }
    }

    "create the correct update AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, _, update, _) =>
        update.code shouldBe "$i++"
        update.lineNumber shouldBe Some(2)
      }
    }

    "create the correct body AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, _, _, body: Block) =>
        body.astChildren.code.l shouldBe List("echo $i")
      }
    }
  }

  "for statements with multiple inits, conditions and updates" should {
    val cpg = code("""<?php
        |for ($i = 0, $j = 100; $i < 42, $j > 42; $i++, $j--) {
        |  echo $i;
        |};
        |""".stripMargin)

    "add a local for the initializer to the enclosing method" in {
      inside(cpg.local.sortBy(_.name).toList) { case List(iLocal, jLocal) =>
        iLocal.name shouldBe "i"
        iLocal.code shouldBe "$i"

        jLocal.name shouldBe "j"
        jLocal.code shouldBe "$j"
      }
    }

    "create the FOR control structure" in {
      inside(cpg.controlStructure.l) { case List(forStructure) =>
        forStructure.controlStructureType shouldBe ControlStructureTypes.FOR
        forStructure.code shouldBe "for ($i = 0,$j = 100;$i < 42,$j > 42;$i++,$j--)"
        forStructure.lineNumber shouldBe Some(2)
      }
    }

    "create the correct initialiser AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(initialisers: Block, _, _, _) =>
        inside(initialisers.astChildren.l) { case List(iInit, jInit) =>
          iInit.code shouldBe "$i = 0"
          iInit.lineNumber shouldBe Some(2)

          jInit.code shouldBe "$j = 100"
          jInit.lineNumber shouldBe Some(2)
        }
      }
    }

    "create the correct condition AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, conditions: Block, _, _) =>
        inside(conditions.astChildren.l) { case List(iCond, jCond) =>
          iCond.code shouldBe "$i < 42"
          iCond.lineNumber shouldBe Some(2)

          jCond.code shouldBe "$j > 42"
          jCond.lineNumber shouldBe Some(2)
        }
      }
    }

    "create the correct update AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, _, updates: Block, _) =>
        inside(updates.astChildren.l) { case List(iUpdate, jUpdate) =>
          iUpdate.code shouldBe "$i++"
          iUpdate.lineNumber shouldBe Some(2)

          jUpdate.code shouldBe "$j--"
          jUpdate.lineNumber shouldBe Some(2)
        }
      }
    }

    "create the correct body AST" in {
      inside(cpg.controlStructure.astChildren.l) { case List(_, _, _, body: Block) =>
        body.astChildren.code.l shouldBe List("echo $i")
      }
    }
  }

  "a full try-catch-finally chain" should {
    val cpg = code("""<?php
        |try {
        |  $body1;
        |} catch (A | D $a) {
        |  $body2;
        |} catch (B $b) {
        |  $body3;
        |} finally {
        |  $body4;
        |};
        |""".stripMargin)

    val List(tryNode) = cpg.controlStructure.isTry.l

    "create the try block correctly" in {
      val List(tryBlock) = tryNode.astChildren.isBlock.l
      tryNode.lineNumber shouldBe Some(2)
      tryBlock.astChildren.code.l shouldBe List("$body1")
    }

    "create the catch blocks correctly" in {
      val List(catchA, catchB) = tryNode.astChildren.isControlStructure.isCatch.l
      catchA.code shouldBe "catch (A | D $a)"
      catchB.code shouldBe "catch (B $b)"

      val List(catchALocal) = catchA.astChildren.isLocal.l
      catchALocal.code shouldBe "a"
      catchALocal.dynamicTypeHintFullName shouldBe IndexedSeq("A", "D")
      catchA.astChildren.isBlock.astChildren.code.l shouldBe List("$body2")
      catchA.lineNumber shouldBe Some(4)

      val List(catchBLocal) = catchB.astChildren.isLocal.l
      catchBLocal.code shouldBe "b"
      catchBLocal.dynamicTypeHintFullName shouldBe IndexedSeq("B")
      catchB.astChildren.isBlock.astChildren.code.l shouldBe List("$body3")
      catchB.lineNumber shouldBe Some(6)
    }

    "create the finally block correctly" in {
      val List(finallyNode) = tryNode.astChildren.isControlStructure.isFinally.l
      finallyNode.astChildren.isBlock.astChildren.code.l shouldBe List("$body4")
      finallyNode.lineNumber shouldBe Some(8)
    }
  }

  "a try-finally chain" should {
    val cpg = code("""<?php
        |try {
        |  $body1;
        |} finally {
        |  $body4;
        |};
        |""".stripMargin)

    val List(tryNode) = cpg.controlStructure.isTry.l

    "create the try block correctly" in {
      val List(tryBlock) = tryNode.astChildren.isBlock.l
      tryNode.lineNumber shouldBe Some(2)
      tryBlock.astChildren.code.l shouldBe List("$body1")
    }

    "create the finally block correctly" in {
      val List(finallyNode) = tryNode.astChildren.isControlStructure.isFinally.l
      finallyNode.astChildren.isBlock.astChildren.code.l shouldBe List("$body4")
      finallyNode.lineNumber shouldBe Some(4)
    }
  }

  "a try-catch chain without finally" should {
    val cpg = code("""<?php
        |try {
        |  $body1;
        |} catch (A $a) {
        |  $body2;
        |} catch (B $b) {
        |  $body3;
        |};
        |""".stripMargin)

    val List(tryNode) = cpg.controlStructure.isTry.l

    "create the try block correctly" in {
      val List(tryBlock) = tryNode.astChildren.isBlock.l
      tryNode.lineNumber shouldBe Some(2)
      tryBlock.astChildren.code.l shouldBe List("$body1")
    }

    "create the catch blocks correctly" in {
      val List(catchA, catchB) = tryNode.astChildren.isControlStructure.isCatch.l
      catchA.astChildren.isBlock.astChildren.code.l shouldBe List("$body2")
      catchA.lineNumber shouldBe Some(4)
      catchB.astChildren.isBlock.astChildren.code.l shouldBe List("$body3")
      catchB.lineNumber shouldBe Some(6)
    }
  }

  "a throw in a try-catch should be created correctly" in {
    val cpg = code("""<?php
        |try {
        |  throw $x;
        |} catch (A $a) {};
        |""".stripMargin)

    inside(cpg.controlStructure.isThrow.l) { case List(throwExpr) =>
      throwExpr.lineNumber shouldBe Some(3)
      throwExpr.code shouldBe "throw $x"
      throwExpr.astChildren.code.l shouldBe List("$x")
    }
  }

  "goto statements and labels" should {
    val cpg = code("""<?php
		 |goto TARGET;
		 |TARGET:
		 |""".stripMargin)

    "create a goto control structure with the correct jump label" in {
      inside(cpg.controlStructure.l) { case List(goto) =>
        goto.controlStructureType shouldBe ControlStructureTypes.GOTO
        goto.code shouldBe "goto TARGET"
        goto.lineNumber shouldBe Some(2)

        inside(goto.astChildren.l) { case List(jumpLabel: JumpLabel) =>
          jumpLabel.name shouldBe "TARGET"
          jumpLabel.code shouldBe "TARGET"
          jumpLabel.lineNumber shouldBe Some(2)
          jumpLabel.order shouldBe 1 // Important for CFG creation
        }
      }
    }

    "create the correct jumpTarget" in {
      inside(cpg.jumpTarget.l) { case List(jumpTarget) =>
        jumpTarget.name shouldBe "TARGET"
        jumpTarget.code shouldBe "TARGET"
        jumpTarget.lineNumber shouldBe Some(3)
      }
    }
  }

  "match expressions" should {
    "work without a default case" in {
      val cpg = code("""<?php
         |match ($condition) {
         |  $a => "A",
         |  $b, $c => "NOT A",
         |};
         |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(matchStructure) =>
        matchStructure.controlStructureType shouldBe ControlStructureTypes.MATCH
        matchStructure.code shouldBe "match ($condition)"
        matchStructure.lineNumber shouldBe Some(2)

        inside(matchStructure.condition.l) { case List(condition: Identifier) =>
          condition.name shouldBe "condition"
          condition.code shouldBe "$condition"
          condition.lineNumber shouldBe Some(2)
        }

        inside(matchStructure.astChildren.collectAll[Block].astChildren.l) {
          case List(
                aTarget: JumpTarget,
                aCond: Identifier,
                aValue: Literal,
                bTarget: JumpTarget,
                bCond: Identifier,
                cTarget: JumpTarget,
                cCond: Identifier,
                otherValue: Literal
              ) =>
            aTarget.code shouldBe "case $a"
            aTarget.lineNumber shouldBe Some(3)

            aCond.code shouldBe "$a"
            aCond.lineNumber shouldBe Some(3)

            aValue.code shouldBe "\"A\""
            aValue.lineNumber shouldBe Some(3)

            bTarget.code shouldBe "case $b"
            bTarget.lineNumber shouldBe Some(4)

            bCond.code shouldBe "$b"
            bCond.lineNumber shouldBe Some(4)

            cTarget.code shouldBe "case $c"
            cTarget.lineNumber shouldBe Some(4)

            cCond.code shouldBe "$c"
            cCond.lineNumber shouldBe Some(4)

            otherValue.code shouldBe "\"NOT A\""
            otherValue.lineNumber shouldBe Some(4)
        }
      }
    }
  }

  "work with a default case" in {
    val cpg = code("""<?php
      |match ($condition) {
      |  $a => "A",
      |  $b, $c => "NOT A",
      |  default => "DEFAULT",
      |};
      |""".stripMargin)

    inside(cpg.controlStructure.l) { case List(matchStructure) =>
      matchStructure.controlStructureType shouldBe ControlStructureTypes.MATCH
      matchStructure.code shouldBe "match ($condition)"
      matchStructure.lineNumber shouldBe Some(2)

      inside(matchStructure.condition.l) { case List(condition: Identifier) =>
        condition.name shouldBe "condition"
        condition.code shouldBe "$condition"
        condition.lineNumber shouldBe Some(2)
      }

      inside(matchStructure.astChildren.collectAll[Block].astChildren.l) {
        case List(
              aTarget: JumpTarget,
              aCond: Identifier,
              aValue: Literal,
              bTarget: JumpTarget,
              bCond: Identifier,
              cTarget: JumpTarget,
              cCond: Identifier,
              otherValue: Literal,
              defaultTarget: JumpTarget,
              defaultValue: Literal
            ) =>
          aTarget.code shouldBe "case $a"
          aTarget.lineNumber shouldBe Some(3)

          aCond.code shouldBe "$a"
          aCond.lineNumber shouldBe Some(3)

          aValue.code shouldBe "\"A\""
          aValue.lineNumber shouldBe Some(3)

          bTarget.code shouldBe "case $b"
          bTarget.lineNumber shouldBe Some(4)

          bCond.code shouldBe "$b"
          bCond.lineNumber shouldBe Some(4)

          cTarget.code shouldBe "case $c"
          cTarget.lineNumber shouldBe Some(4)

          cCond.code shouldBe "$c"
          cCond.lineNumber shouldBe Some(4)

          otherValue.code shouldBe "\"NOT A\""
          otherValue.lineNumber shouldBe Some(4)

          defaultTarget.code shouldBe "default"
          defaultTarget.lineNumber shouldBe Some(5)

          defaultValue.code shouldBe "\"DEFAULT\""
          defaultValue.lineNumber shouldBe Some(5)
      }
    }
  }

  "yield from should be represented as a yield with the correct code field" in {
    val cpg = code("""<?php
     |function foo($xs) {
     |  yield from $xs;
     |}
     |""".stripMargin)

    inside(cpg.controlStructure.l) { case List(yieldStructure) =>
      yieldStructure.controlStructureType shouldBe ControlStructureTypes.YIELD
      yieldStructure.code shouldBe "yield from $xs"
      yieldStructure.lineNumber shouldBe Some(3)

      inside(yieldStructure.astChildren.l) { case List(xs: Identifier) =>
        xs.name shouldBe "xs"
        xs.code shouldBe "$xs"
        xs.lineNumber shouldBe Some(3)
      }
    }
  }

  "yield expressions" should {
    "be created when they have no value" in {
      val cpg = code("""<?php
       |function foo() {
       |  yield;
       |}
       |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(yieldStructure) =>
        yieldStructure.controlStructureType shouldBe ControlStructureTypes.YIELD
        yieldStructure.code shouldBe "yield"
        yieldStructure.lineNumber shouldBe Some(3)

        yieldStructure.astChildren.size shouldBe 0
      }
    }

    "be created when they have values without keys" in {
      val cpg = code("""<?php
        |function foo() {
        |  yield 1;
        |}
        |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(yieldStructure) =>
        yieldStructure.controlStructureType shouldBe ControlStructureTypes.YIELD
        yieldStructure.code shouldBe "yield 1"
        yieldStructure.lineNumber shouldBe Some(3)

        inside(yieldStructure.astChildren.l) { case List(value: Literal) =>
          value.code shouldBe "1"
          value.lineNumber shouldBe Some(3)
        }
      }
    }

    "be created when they have values with keys" in {
      val cpg = code("""<?php
        |function foo($x) {
        |  yield 1 => $x;
        |}
        |""".stripMargin)

      inside(cpg.controlStructure.l) { case List(yieldStructure) =>
        yieldStructure.controlStructureType shouldBe ControlStructureTypes.YIELD
        yieldStructure.code shouldBe "yield 1 => $x"
        yieldStructure.lineNumber shouldBe Some(3)

        inside(yieldStructure.astChildren.l) { case List(key: Literal, value: Identifier) =>
          key.code shouldBe "1"
          key.lineNumber shouldBe Some(3)

          value.name shouldBe "x"
          value.code shouldBe "$x"
          value.lineNumber shouldBe Some(3)
        }
      }
    }
  }

  "foreach statements should not create parentless identifiers" in {
    val cpg = code("""<?php
        |foreach($GLOBALS as $x) {};
        |""".stripMargin)
    cpg.all.collectAll[Identifier].filter(node => Try(node.astParent).isFailure).toList shouldBe Nil
  }

  "foreach statements referencing parameters in methods should not create parentless identifiers" in {
    val cpg = code("""<?php
                     |class Test {
                     |  function test() {
                     |    foreach($this as $x) {}
                     |  }
                     |}
                     |""".stripMargin)
    cpg.all.collectAll[Identifier].filter(node => Try(node.astParent).isFailure).toList shouldBe Nil
  }

  "foreach statements referencing regular parameters should not create parentless identifiers" in {
    val cpg = code("""<?php
                     |function test($values) {
                     |  foreach($values as $x) {}
                     |}
                     |""".stripMargin)
    cpg.all.collectAll[Identifier].filter(node => Try(node.astParent).isFailure).toList shouldBe Nil
  }

  "foreach statements referencing locals should not create parentless identifiers" in {
    val cpg = code("""<?php
                     |function test() {
                     |  $values = 2;
                     |  foreach($values as $x) {}
                     |}
                     |""".stripMargin)
    cpg.all.collectAll[Identifier].filter(node => Try(node.astParent).isFailure).toList shouldBe Nil
  }

  "foreach statements with only simple values should be represented as a for" in {
    val cpg = code("""<?php
     |function foo($arr) {
     |  foreach ($arr as $val) {
     |    echo $val;
     |  }
     |}
     |""".stripMargin)

    val foreachStruct = inside(cpg.method.name("foo").body.astChildren.l) {
      case List(iterLocal: Local, valLocal: Local, foreachStruct: ControlStructure) =>
        iterLocal.name shouldBe "foo@iter_tmp-0"
        valLocal.name shouldBe "val"
        foreachStruct
    }

    foreachStruct.code shouldBe "foreach ($arr as $val)"

    val (initAsts, conditionAst, updateAsts, body) = inside(foreachStruct.astChildren.l) {
      case List(initAsts: Block, conditionAst: Call, updateAsts: Block, body: Block) =>
        (initAsts, conditionAst, updateAsts, body)
    }

    inside(initAsts.astChildren.l) { case List(iterInit: Call, valInitBlock: Block) =>
      iterInit.name shouldBe Operators.assignment
      iterInit.code shouldBe "$foo@iter_tmp-0 = $arr"
      inside(iterInit.argument.l) { case List(iterTemp: Identifier, iterExpr: Identifier) =>
        iterTemp.name shouldBe "foo@iter_tmp-0"
        iterTemp.code shouldBe "$foo@iter_tmp-0"
        iterTemp.argumentIndex shouldBe 1

        iterExpr.name shouldBe "arr"
        iterExpr.code shouldBe "$arr"
        iterExpr.argumentIndex shouldBe 2
      }

      val List(valInit: Call) = valInitBlock.astChildren.l: @unchecked
      valInit.name shouldBe Operators.assignment
      valInit.code shouldBe "$val = $foo@iter_tmp-0->current()"
      inside(valInit.argument.l) { case List(valId: Identifier, currentCall: Call) =>
        valId.name shouldBe "val"
        valId.code shouldBe "$val"
        valId.argumentIndex shouldBe 1

        currentCall.name shouldBe "current"
        currentCall.methodFullName shouldBe s"Iterator.current"
        currentCall.code shouldBe "$foo@iter_tmp-0->current()"
        inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
          iterRecv.name shouldBe "foo@iter_tmp-0"
          iterRecv.argumentIndex shouldBe 0
        }
      }
    }

    conditionAst.name shouldBe Operators.logicalNot
    conditionAst.code shouldBe "!is_null($val)"
    inside(conditionAst.astChildren.l) { case List(isNullCall: Call) =>
      isNullCall.name shouldBe "is_null"
      isNullCall.code shouldBe "is_null($val)"
    }

    inside(updateAsts.astChildren.l) { case List(nextCall: Call, valAssignBlock: Block) =>
      nextCall.name shouldBe "next"
      nextCall.methodFullName shouldBe "Iterator.next"
      nextCall.code shouldBe "$foo@iter_tmp-0->next()"
      inside(nextCall.argument(0).start.l) { case List(iterTmp: Identifier) =>
        iterTmp.name shouldBe "foo@iter_tmp-0"
        iterTmp.code shouldBe "$foo@iter_tmp-0"
        iterTmp.argumentIndex shouldBe 0
      }

      val List(valAssign: Call) = valAssignBlock.astChildren.l: @unchecked
      valAssign.name shouldBe Operators.assignment
      valAssign.code shouldBe "$val = $foo@iter_tmp-0->current()"
    }

    inside(body.astChildren.l) { case List(echoCall: Call) =>
      echoCall.code shouldBe "echo $val"
    }
  }

  "foreach statements with assignments by ref should be represented as a for" in {
    val cpg = code("""<?php
      |function foo($arr) {
      |  foreach ($arr as &$val) {
      |    echo $val;
      |  }
      |}
      |""".stripMargin)

    val foreachStruct = inside(cpg.method.name("foo").body.astChildren.l) {
      case List(iterLocal: Local, valLocal: Local, foreachStruct: ControlStructure) =>
        iterLocal.name shouldBe "foo@iter_tmp-0"
        valLocal.name shouldBe "val"

        foreachStruct
    }

    foreachStruct.code shouldBe "foreach ($arr as &$val)"

    val (initAsts, updateAsts, body) = inside(foreachStruct.astChildren.l) {
      case List(initAsts: Block, _, updateAsts: Block, body: Block) =>
        (initAsts, updateAsts, body)
    }

    inside(initAsts.astChildren.l) { case List(_: Call, valInitBlock: Block) =>
      val List(valInit: Call) = valInitBlock.astChildren.l: @unchecked
      valInit.name shouldBe Operators.assignment
      valInit.code shouldBe "$val = &$foo@iter_tmp-0->current()"
      inside(valInit.argument.l) { case List(valId: Identifier, addressOfCall: Call) =>
        valId.name shouldBe "val"
        valId.code shouldBe "$val"
        valId.argumentIndex shouldBe 1

        addressOfCall.name shouldBe Operators.addressOf
        addressOfCall.code shouldBe "&$foo@iter_tmp-0->current()"

        inside(addressOfCall.argument.l) { case List(currentCall: Call) =>
          currentCall.name shouldBe "current"
          currentCall.methodFullName shouldBe s"Iterator.current"
          currentCall.code shouldBe "$foo@iter_tmp-0->current()"
          inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
            iterRecv.name shouldBe "foo@iter_tmp-0"
            iterRecv.argumentIndex shouldBe 0
          }
        }
      }
    }

    inside(updateAsts.astChildren.l) { case List(_: Call, valAssignBlock: Block) =>
      val List(valAssign: Call) = valAssignBlock.astChildren.l: @unchecked
      valAssign.name shouldBe Operators.assignment
      valAssign.code shouldBe "$val = &$foo@iter_tmp-0->current()"
    }

    inside(body.astChildren.l) { case List(echoCall: Call) =>
      echoCall.code shouldBe "echo $val"
    }
  }

  "foreach statements with key-val should be represented as a for" in {
    val cpg = code("""<?php
      |function foo($arr) {
      |  foreach ($arr as $key => $val) {
      |    echo $val;
      |  }
      |}
      |""".stripMargin)

    val foreachStruct = inside(cpg.method.name("foo").body.astChildren.l) {
      case List(iterLocal: Local, keyLocal: Local, valLocal: Local, foreachStruct: ControlStructure) =>
        iterLocal.name shouldBe "foo@iter_tmp-0"
        keyLocal.name shouldBe "key"
        valLocal.name shouldBe "val"

        foreachStruct
    }

    foreachStruct.code shouldBe "foreach ($arr as $key => $val)"

    val (initAsts, updateAsts, body) = inside(foreachStruct.astChildren.l) {
      case List(initAsts: Block, _, updateAsts: Block, body: Block) =>
        (initAsts, updateAsts, body)
    }

    inside(initAsts.assignment.l) { case List(_: Call, keyInit: Call, valInit: Call) =>
      keyInit.name shouldBe Operators.assignment
      keyInit.code shouldBe "$key = $foo@iter_tmp-0->key()"
      inside(keyInit.argument.l) { case List(target: Identifier, keyCall: Call) =>
        target.name shouldBe "key"
        keyCall.name shouldBe "key"
        keyCall.methodFullName shouldBe s"Iterator.key"
        keyCall.code shouldBe "$foo@iter_tmp-0->key()"
        inside(keyCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
          iterRecv.name shouldBe "foo@iter_tmp-0"
          iterRecv.argumentIndex shouldBe 0
        }
      }

      valInit.name shouldBe Operators.assignment
      valInit.code shouldBe "$val = $foo@iter_tmp-0->current()"
      inside(valInit.argument.l) { case List(target: Identifier, currentCall: Call) =>
        target.name shouldBe "val"
        currentCall.name shouldBe "current"
        currentCall.methodFullName shouldBe s"Iterator.current"
        currentCall.code shouldBe "$foo@iter_tmp-0->current()"
        inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
          iterRecv.name shouldBe "foo@iter_tmp-0"
          iterRecv.argumentIndex shouldBe 0
        }
      }
    }

    inside(updateAsts.astChildren.l) { case List(_: Call, updateBlock: Block) =>
      val tmp = updateBlock.astChildren.l
      inside(updateBlock.assignment.l) { case List(keyInit: Call, valInit: Call) =>
        keyInit.code shouldBe "$key = $foo@iter_tmp-0->key()"
        valInit.code shouldBe "$val = $foo@iter_tmp-0->current()"
      }
    }

    inside(body.astChildren.l) { case List(echoCall: Call) =>
      echoCall.code shouldBe "echo $val"
    }
  }

  "foreach loop with listExpr" should {
    val cpg = code("""<?php
        |function foo($data) {
        | foreach($data as list($a, $b, $c)) {
        |   echo $a;
        |   echo $b;
        | }
        |}
        |""".stripMargin)

    def assignmentTests(assignmentCall: Call, variableName: String, arrayIndex: Int): Unit = {
      assignmentCall.methodFullName shouldBe Operators.assignment

      inside(assignmentCall.argument.l) { case List(lhsIdent: Identifier, rhsIdxAccess: Call) =>
        lhsIdent.name shouldBe variableName
        lhsIdent.code shouldBe s"$$${variableName}"

        rhsIdxAccess.methodFullName shouldBe Operators.indexAccess
        rhsIdxAccess.code shouldBe s"$$foo@tmp-0[${arrayIndex}]" // $foo@tmp-0 is the name of the variable holding the current value
        val List(idxLhs: Identifier, idxRhs: Literal) = rhsIdxAccess.argument.l: @unchecked
        idxLhs.name shouldBe "foo@tmp-0"
        idxLhs.code shouldBe "$foo@tmp-0"
      }
    }

    "foreach structure" in {
      inside(cpg.method.name("foo").body.astChildren.l) {
        case (iterLocal: Local) :: (aLocal: Local) :: (bLocal: Local) :: (cLocal: Local) :: (foreachStruct: ControlStructure) :: Nil =>
          iterLocal.name shouldBe "foo@iter_tmp-0"
          aLocal.name shouldBe "a"
          bLocal.name shouldBe "b"
          cLocal.name shouldBe "c"
          foreachStruct.code shouldBe "foreach ($data as list($a,$b,$c))"
        case xs => fail(s"Expected five astChildren, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Init AST structures" in {
      inside(cpg.method.name("foo").body.astChildren.isControlStructure.astChildren.l) {
        case (initAsts: Block) :: _ :: _ :: _ =>
          inside(initAsts.astChildren.l) { case List(iterInit: Call, valInitBlock: Block) =>
            iterInit.name shouldBe Operators.assignment
            iterInit.code shouldBe "$foo@iter_tmp-0 = $data"
            inside(iterInit.argument.l) { case List(iterTemp: Identifier, iterExpr: Identifier) =>
              iterTemp.name shouldBe "foo@iter_tmp-0"
              iterTemp.code shouldBe "$foo@iter_tmp-0"
              iterTemp.argumentIndex shouldBe 1

              iterExpr.name shouldBe "data"
              iterExpr.code shouldBe "$data"
              iterExpr.argumentIndex shouldBe 2
            }

            val List(valInit: Call, aInit: Call, bInit: Call, cInit: Call) =
              valInitBlock.astChildren.isBlock.astChildren.l: @unchecked
            valInit.name shouldBe Operators.assignment
            valInit.code shouldBe "$foo@tmp-0 = $foo@iter_tmp-0->current()"
            inside(valInit.argument.l) { case List(valId: Identifier, currentCall: Call) =>
              valId.name shouldBe "foo@tmp-0"
              valId.code shouldBe "$foo@tmp-0"
              valId.argumentIndex shouldBe 1

              currentCall.name shouldBe "current"
              currentCall.methodFullName shouldBe s"Iterator.current"
              currentCall.code shouldBe "$foo@iter_tmp-0->current()"
              inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
                iterRecv.name shouldBe "foo@iter_tmp-0"
                iterRecv.argumentIndex shouldBe 0
              }
            }

            assignmentTests(aInit, variableName = "a", arrayIndex = 0)
            assignmentTests(bInit, variableName = "b", arrayIndex = 1)
            assignmentTests(cInit, variableName = "c", arrayIndex = 2)
          }
        case xs => fail(s"Expected four children for control structure, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Update AST structures" in {
      inside(cpg.method.name("foo").body.astChildren.isControlStructure.astChildren.l) {
        case _ :: _ :: (updateAsts: Block) :: (body: Block) :: Nil =>
          inside(updateAsts.astChildren.l) { case List(nextCall: Call, valAssignBlock: Block) =>
            nextCall.name shouldBe "next"
            nextCall.methodFullName shouldBe "Iterator.next"
            nextCall.code shouldBe "$foo@iter_tmp-0->next()"
            inside(nextCall.argument(0).start.l) { case List(iterTmp: Identifier) =>
              iterTmp.name shouldBe "foo@iter_tmp-0"
              iterTmp.code shouldBe "$foo@iter_tmp-0"
              iterTmp.argumentIndex shouldBe 0
            }

            val List(valInit: Call, aInit: Call, bInit: Call, cInit: Call) =
              valAssignBlock.astChildren.isBlock.astChildren.l: @unchecked
            valInit.name shouldBe Operators.assignment
            valInit.code shouldBe "$foo@tmp-0 = $foo@iter_tmp-0->current()"
            inside(valInit.argument.l) { case List(valId: Identifier, currentCall: Call) =>
              valId.name shouldBe "foo@tmp-0"
              valId.code shouldBe "$foo@tmp-0"
              valId.argumentIndex shouldBe 1

              currentCall.name shouldBe "current"
              currentCall.methodFullName shouldBe s"Iterator.current"
              currentCall.code shouldBe "$foo@iter_tmp-0->current()"
              inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
                iterRecv.name shouldBe "foo@iter_tmp-0"
                iterRecv.argumentIndex shouldBe 0
              }
            }

            assignmentTests(aInit, variableName = "a", arrayIndex = 0)
            assignmentTests(bInit, variableName = "b", arrayIndex = 1)
            assignmentTests(cInit, variableName = "c", arrayIndex = 2)
          }

          inside(body.astChildren.l) { case List(echoCallA: Call, echoCallB: Call) =>
            echoCallA.code shouldBe "echo $a"
            echoCallB.code shouldBe "echo $b"
          }
        case xs => fail(s"Expected four children for control structure, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "create OR `!isNull` checks" in {
      /*
      ```php
      $data = [[1, 2]]
      foreach($data as list($a, $b, $c)
      ```
      The above sample is still valid Php code and runs as long as we don't use the $c variable in the loop (only a warning is emitted when running the code). This means
      that the `!isNull` condition should be an OR since the loop still executes above, even though $c is a null-value
       */
      inside(cpg.controlStructure.isFor.condition.l) {
        case parentOr :: Nil =>
          inside(parentOr.astChildren.l) {
            case (nestedOr: Call) :: (logicalNot: Call) :: Nil =>
              nestedOr.methodFullName shouldBe Operators.or
              val List(orLeftArg: Call, orRightArg: Call) = nestedOr.argument.l: @unchecked
              orLeftArg.methodFullName shouldBe Operators.logicalNot
              orRightArg.methodFullName shouldBe Operators.logicalNot

              val List(orLeftArgNullCall: Call) = orLeftArg.argument.l: @unchecked
              orLeftArgNullCall.methodFullName shouldBe PhpOperators.isNull

              val List(aVar) = orLeftArgNullCall.argument.l: @unchecked
              aVar.code shouldBe "$a"

              val List(orRightArgNullCall: Call) = orRightArg.argument.l: @unchecked
              orRightArgNullCall.methodFullName shouldBe PhpOperators.isNull

              val List(bVar) = orRightArgNullCall.argument.l: @unchecked
              bVar.code shouldBe "$b"

              logicalNot.methodFullName shouldBe Operators.logicalNot
              val List(notArg: Call) = logicalNot.argument.l: @unchecked
              notArg.methodFullName shouldBe PhpOperators.isNull

              val List(cVar: Identifier) = notArg.argument.l: @unchecked
              cVar.code shouldBe "$c"

            case xs => fail(s"Expected two children calls for condition, got, ${xs.code.mkString("[", ",", "]")}")
          }
        case xs => fail(s"Expected one condition for FOR construct, got ${xs.code.mkString("[", ",", "]")}")
      }
    }
  }

  "foreach loop with nested list construct" should {
    val cpg = code("""<?php
                     |function foo($data) {
                     | foreach($data as list($a, list($b, $c))) {
                     |   echo $a;
                     |   echo $b;
                     | }
                     |}
                     |""".stripMargin)
    def assignmentTests(assignmentCall: Call, variableName: String, tmpIndex: Int, arrayIndex: Int): Unit = {
      assignmentCall.methodFullName shouldBe Operators.assignment

      inside(assignmentCall.argument.l) { case List(lhsIdent: Identifier, rhsIdxAccess: Call) =>
        lhsIdent.name shouldBe variableName
        lhsIdent.code shouldBe s"$$${variableName}"

        rhsIdxAccess.methodFullName shouldBe Operators.indexAccess
        rhsIdxAccess.code shouldBe s"$$foo@tmp-${tmpIndex}[${arrayIndex}]" // $foo@tmp-0 is the name of the variable holding the current value
        val List(idxLhs: Identifier, idxRhs: Literal) = rhsIdxAccess.argument.l: @unchecked
        idxLhs.name shouldBe s"foo@tmp-${tmpIndex}"
        idxLhs.code shouldBe s"$$foo@tmp-${tmpIndex}"
      }
    }

    "foreach structure" in {
      inside(cpg.method.name("foo").body.astChildren.l) {
        case (iterLocal: Local) :: (aLocal: Local) :: (bLocal: Local) :: (cLocal: Local) :: (foreachStruct: ControlStructure) :: Nil =>
          iterLocal.name shouldBe "foo@iter_tmp-0"
          aLocal.name shouldBe "a"
          bLocal.name shouldBe "b"
          cLocal.name shouldBe "c"
          foreachStruct.code shouldBe "foreach ($data as list($a,list($b,$c)))"
        case xs => fail(s"Expected five astChildren, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Init AST structures" in {
      inside(cpg.method.name("foo").body.astChildren.isControlStructure.astChildren.l) {
        case (initAsts: Block) :: _ :: _ :: _ =>
          inside(initAsts.astChildren.l) { case List(iterInit: Call, valInitBlock: Block) =>
            iterInit.name shouldBe Operators.assignment
            iterInit.code shouldBe "$foo@iter_tmp-0 = $data"
            inside(iterInit.argument.l) { case List(iterTemp: Identifier, iterExpr: Identifier) =>
              iterTemp.name shouldBe "foo@iter_tmp-0"
              iterTemp.code shouldBe "$foo@iter_tmp-0"
              iterTemp.argumentIndex shouldBe 1

              iterExpr.name shouldBe "data"
              iterExpr.code shouldBe "$data"
              iterExpr.argumentIndex shouldBe 2
            }

            val List(valInit: Call, aInit: Call, tmp1Init: Call, tmp2Init: Call, bInit: Call, cInit: Call) =
              valInitBlock.astChildren.isBlock.astChildren.l: @unchecked
            valInit.name shouldBe Operators.assignment
            valInit.code shouldBe "$foo@tmp-0 = $foo@iter_tmp-0->current()"
            inside(valInit.argument.l) { case List(valId: Identifier, currentCall: Call) =>
              valId.name shouldBe "foo@tmp-0"
              valId.code shouldBe "$foo@tmp-0"
              valId.argumentIndex shouldBe 1

              currentCall.name shouldBe "current"
              currentCall.methodFullName shouldBe s"Iterator.current"
              currentCall.code shouldBe "$foo@iter_tmp-0->current()"
              inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
                iterRecv.name shouldBe "foo@iter_tmp-0"
                iterRecv.argumentIndex shouldBe 0
              }
            }

            tmp2Init.methodFullName shouldBe Operators.assignment
            tmp2Init.code shouldBe "$foo@tmp-2 = $foo@tmp-1"
            inside(tmp2Init.argument.l) { case List(lhsIdent: Identifier, rhsIdent: Identifier) =>
              lhsIdent.name shouldBe "foo@tmp-2"
              lhsIdent.code shouldBe "$foo@tmp-2"

              rhsIdent.name shouldBe "foo@tmp-1"
              rhsIdent.code shouldBe "$foo@tmp-1"
            }

            assignmentTests(aInit, variableName = "a", tmpIndex = 0, arrayIndex = 0)
            assignmentTests(bInit, variableName = "b", tmpIndex = 2, arrayIndex = 0)
            assignmentTests(cInit, variableName = "c", tmpIndex = 2, arrayIndex = 1)

            assignmentTests(tmp1Init, variableName = "foo@tmp-1", tmpIndex = 0, arrayIndex = 1)
          }
        case xs => fail(s"Expected four children for control structure, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "Update AST structures" in {
      inside(cpg.method.name("foo").body.astChildren.isControlStructure.astChildren.l) {
        case _ :: _ :: (updateAsts: Block) :: (body: Block) :: Nil =>
          inside(updateAsts.astChildren.l) { case List(nextCall: Call, valAssignBlock: Block) =>
            nextCall.name shouldBe "next"
            nextCall.methodFullName shouldBe "Iterator.next"
            nextCall.code shouldBe "$foo@iter_tmp-0->next()"
            inside(nextCall.argument(0).start.l) { case List(iterTmp: Identifier) =>
              iterTmp.name shouldBe "foo@iter_tmp-0"
              iterTmp.code shouldBe "$foo@iter_tmp-0"
              iterTmp.argumentIndex shouldBe 0
            }

            val List(valInit: Call, aInit: Call, tmp1Init: Call, tmp2Init: Call, bInit: Call, cInit: Call) =
              valAssignBlock.astChildren.isBlock.astChildren.l: @unchecked
            valInit.name shouldBe Operators.assignment
            valInit.code shouldBe "$foo@tmp-0 = $foo@iter_tmp-0->current()"
            inside(valInit.argument.l) { case List(valId: Identifier, currentCall: Call) =>
              valId.name shouldBe "foo@tmp-0"
              valId.code shouldBe "$foo@tmp-0"
              valId.argumentIndex shouldBe 1

              currentCall.name shouldBe "current"
              currentCall.methodFullName shouldBe s"Iterator.current"
              currentCall.code shouldBe "$foo@iter_tmp-0->current()"
              inside(currentCall.argument(0).start.l) { case List(iterRecv: Identifier) =>
                iterRecv.name shouldBe "foo@iter_tmp-0"
                iterRecv.argumentIndex shouldBe 0
              }
            }

            tmp2Init.methodFullName shouldBe Operators.assignment
            tmp2Init.code shouldBe "$foo@tmp-2 = $foo@tmp-1"
            inside(tmp2Init.argument.l) { case List(lhsIdent: Identifier, rhsIdent: Identifier) =>
              lhsIdent.name shouldBe "foo@tmp-2"
              lhsIdent.code shouldBe "$foo@tmp-2"

              rhsIdent.name shouldBe "foo@tmp-1"
              rhsIdent.code shouldBe "$foo@tmp-1"
            }

            assignmentTests(aInit, variableName = "a", tmpIndex = 0, arrayIndex = 0)
            assignmentTests(bInit, variableName = "b", tmpIndex = 2, arrayIndex = 0)
            assignmentTests(cInit, variableName = "c", tmpIndex = 2, arrayIndex = 1)

            assignmentTests(tmp1Init, variableName = "foo@tmp-1", tmpIndex = 0, arrayIndex = 1)
          }

          inside(body.astChildren.l) { case List(echoCallA: Call, echoCallB: Call) =>
            echoCallA.code shouldBe "echo $a"
            echoCallB.code shouldBe "echo $b"
          }
        case xs => fail(s"Expected four children for control structure, got ${xs.code.mkString("[", ",", "]")}")
      }
    }

    "create OR `!isNull` checks" in {
      /*
      For nested list deconstruction, we have !a || (!b || !c) instead of (!a || !b) || !c because we process
      the nested list first, which gives the (!b || !c), and then we use that result to create the !a || (!b || !c)
       */
      inside(cpg.controlStructure.isFor.condition.l) {
        case parentOr :: Nil =>
          inside(parentOr.astChildren.l) {
            case (logicalNot: Call) :: (nestedOr: Call) :: Nil =>
              nestedOr.methodFullName shouldBe Operators.or
              val List(orLeftArg: Call, orRightArg: Call) = nestedOr.argument.l: @unchecked
              orLeftArg.methodFullName shouldBe Operators.logicalNot
              orRightArg.methodFullName shouldBe Operators.logicalNot

              val List(orLeftArgNullCall: Call) = orLeftArg.argument.l: @unchecked
              orLeftArgNullCall.methodFullName shouldBe PhpOperators.isNull

              val List(bVar) = orLeftArgNullCall.argument.l: @unchecked
              bVar.code shouldBe "$b"

              val List(orRightArgNullCall: Call) = orRightArg.argument.l: @unchecked
              orRightArgNullCall.methodFullName shouldBe PhpOperators.isNull

              val List(cVar) = orRightArgNullCall.argument.l: @unchecked
              cVar.code shouldBe "$c"

              logicalNot.methodFullName shouldBe Operators.logicalNot
              val List(notArg: Call) = logicalNot.argument.l: @unchecked
              notArg.methodFullName shouldBe PhpOperators.isNull

              val List(aVar: Identifier) = notArg.argument.l: @unchecked
              aVar.code shouldBe "$a"

            case xs => fail(s"Expected two children calls for condition, got, ${xs.code.mkString("[", ",", "]")}")
          }
        case xs => fail(s"Expected one condition for FOR construct, got ${xs.code.mkString("[", ",", "]")}")
      }
    }
  }
}
