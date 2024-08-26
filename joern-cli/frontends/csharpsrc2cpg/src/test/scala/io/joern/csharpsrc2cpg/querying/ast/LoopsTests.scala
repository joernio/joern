package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Local}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class LoopsTests extends CSharpCode2CpgFixture(withDataFlow = true) {
  "AST Creation for loops" should {
    "be correct for foreach statement" in {
      val cpg = code(basicBoilerplate("""
          |List<int> fibNumbers = [0, 1, 1, 2, 3, 5, 8, 13];
          |foreach (int element in fibNumbers)
          |{
          |    Console.Write($"{element} ");
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.l) {
        case forEachNode :: Nil =>
          forEachNode.controlStructureType shouldBe ControlStructureTypes.FOR

          inside(forEachNode.astChildren.l) {
            case (idxLocal: Local) :: (elementLocal: Local) :: (initAssign: Call) :: (cond: Call) :: (update: Call) :: (forBlock: Block) :: Nil =>
              idxLocal.name shouldBe "_idx_"
              idxLocal.typeFullName shouldBe "System.Int32"

              elementLocal.name shouldBe "element"
              elementLocal.typeFullName shouldBe "System.Int32"

              initAssign.code shouldBe "_idx_ = 0"
              initAssign.name shouldBe Operators.assignment
              initAssign.methodFullName shouldBe Operators.assignment

              cond.code shouldBe "_idx_ < fibNumbers.Count"
              cond.name shouldBe Operators.lessThan
              cond.methodFullName shouldBe Operators.lessThan

              update.code shouldBe "element = fibNumbers[_idx_++]"
              update.name shouldBe Operators.assignment
              update.methodFullName shouldBe Operators.assignment

              val List(writeCall) = cpg.call.nameExact("Write").l
              writeCall.astParent shouldBe forBlock
          }

        case _ => fail("No control structure node found for `foreach`.")
      }
    }

    "be correct for `for` statement (case 1)" in {
      val cpg = code(basicBoilerplate("""
          |for (int i = 0; i < 10;i++) {
          | Console.Write(i);
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.FOR).l) {
        case forNode :: Nil =>
          forNode.controlStructureType shouldBe ControlStructureTypes.FOR
          forNode.code shouldBe "for (int i = 0;i < 10;i++)"
          forNode.astParent shouldBe cpg.method("Main").astChildren.isBlock.l.head

          inside(forNode.astChildren.isCall.l) {
            case initNode :: conditionNode :: incrementorNode :: Nil =>
              initNode.code shouldBe "i = 0"
              conditionNode.code shouldBe "i < 10"
              incrementorNode.code shouldBe "i++"
            case _ => fail("correct number of calls not found.")
          }

          inside(forNode.astChildren.isLocal.l) {
            case initNode :: Nil =>
              initNode.code shouldBe "i"
              initNode.typeFullName shouldBe "System.Int32"
              initNode.astParent shouldBe forNode
            case _ => fail("No local node found as initializer.")

          }

          inside(forNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(writeCall) = cpg.call("Write").l
              writeCall.astParent shouldBe blockNode
              blockNode.astParent shouldBe forNode
            case _ => fail("No child blockNode found.")

          }
        case _ => fail("No for loop control structure found.")
      }
    }

    "be correct for `for` statement (case 2)" in {
      val cpg = code(basicBoilerplate("""
          |for (;;) {
          | Console.Write(i);
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.FOR).l) {
        case forNode :: Nil =>
          forNode.controlStructureType shouldBe ControlStructureTypes.FOR
          forNode.code shouldBe "for (;;)"
          forNode.astParent shouldBe cpg.method("Main").astChildren.isBlock.l.head

          inside(forNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(writeCall) = cpg.call("Write").l
              writeCall.astParent shouldBe blockNode
              blockNode.astParent shouldBe forNode
            case _ => fail("No child blockNode found.")

          }
        case _ => fail("No for loop control structure found.")
      }
    }

    "be correct for do..while statement" in {
      val cpg = code(basicBoilerplate("""
          |int n = 0;
          |do
          |{
          |    Console.Write(n);
          |    n++;
          |} while (n < 5);
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.DO).l) {
        case doNode :: Nil =>
          doNode.code shouldBe "do {...} while (n < 5)"
          doNode.controlStructureType shouldBe ControlStructureTypes.DO
          doNode.astParent shouldBe cpg.method("Main").astChildren.isBlock.head

          inside(doNode.astChildren.isBlock.l) {
            case doBlock :: Nil =>
              val List(writeCall) = cpg.call.nameExact("Write").l
              val List(incCall)   = cpg.assignment.codeExact("n++").l
              writeCall.astParent shouldBe doBlock
              incCall.astParent shouldBe doBlock
            case _ => fail("Block node not found inside do loop.")

          }

          inside(doNode.astChildren.isCall.name(Operators.lessThan).l) {
            case lessThanExpression :: Nil =>
              lessThanExpression.code shouldBe "n < 5"
            case _ => fail("Condition not found for do loop.")
          }

        case _ => fail("No do loop found in the method.")
      }
    }

    "be correct for while statement" in {
      val cpg = code(basicBoilerplate("""
          |int n = 0;
          |while (n < 5)
          |{
          |    Console.Write(n);
          |    n++;
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.WHILE).l) {
        case whileNode :: Nil =>
          whileNode.code shouldBe "while (n < 5)"
          whileNode.controlStructureType shouldBe ControlStructureTypes.WHILE
          whileNode.astParent shouldBe cpg.method("Main").astChildren.isBlock.head

          inside(whileNode.astChildren.isBlock.l) {
            case whileBlock :: Nil =>
              val List(writeCall) = cpg.call.nameExact("Write").l
              val List(incCall)   = cpg.assignment.codeExact("n++").l
              writeCall.astParent shouldBe whileBlock
              incCall.astParent shouldBe whileBlock
            case _ => fail("Block node not found inside while loop.")

          }

          inside(whileNode.astChildren.isCall.name(Operators.lessThan).l) {
            case lessThanExpression :: Nil =>
              lessThanExpression.code shouldBe "n < 5"
            case _ => fail("Condition not found for while loop.")
          }

        case _ => fail("No while loop found in the method.")
      }
    }

  }
}
