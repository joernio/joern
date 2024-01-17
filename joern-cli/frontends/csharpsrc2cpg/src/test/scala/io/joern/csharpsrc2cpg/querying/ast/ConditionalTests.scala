package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.nodes.{ControlStructure, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class ConditionalTests extends CSharpCode2CpgFixture {

  "AST Creation for conditionals" should {
    "be correct for if statements" in {
      val cpg = code(basicBoilerplate("""
          |int a = 1;
          |if (a == 1) {
          | a++;
          |}
          |""".stripMargin))

      cpg.method("Main").controlStructure.size shouldBe 1

      inside(cpg.method("Main").controlStructure.l) {
        case ifNode :: Nil =>
          ifNode.code shouldBe "if (a == 1)"
          ifNode.controlStructureType shouldBe ControlStructureTypes.IF
          inside(ifNode.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "a == 1"
          }
          ifNode.whenTrue.assignment.code.l shouldBe List("a++")

          inside(ifNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(incCall) = blockNode.ast.isCall.l
              incCall.code shouldBe "a++"
              incCall.astParent shouldBe blockNode
            case _ => fail("Incorrect body for `if` statement")
          }

        case _ => fail("No `if` node found.")
      }
    }

    "be correct for if-else statements" in {
      val cpg = code(basicBoilerplate("""
          |int a = 1;
          |if (a == 1) {
          | a++;
          |} else {
          | a--;
          |}
          |""".stripMargin))
      cpg.method("Main").controlStructure.size shouldBe 2

      inside(cpg.method("Main").controlStructure.l) {
        case ifNode :: elseNode :: Nil =>
          ifNode.code shouldBe "if (a == 1)"
          elseNode.code shouldBe "else"

          ifNode.controlStructureType shouldBe ControlStructureTypes.IF
          elseNode.controlStructureType shouldBe ControlStructureTypes.ELSE

          inside(ifNode.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "a == 1"
          }

          inside(ifNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(incCall) = blockNode.ast.isCall.l
              incCall.code shouldBe "a++"
              incCall.astParent shouldBe blockNode
            case _ => fail("Incorect body for `if` statement")
          }

          inside(elseNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(decCall) = blockNode.ast.isCall.l
              decCall.code shouldBe "a--"
              decCall.astParent shouldBe blockNode
            case _ => fail("Incorect body for `else` statement")

          }

          ifNode.whenTrue.assignment.code.l shouldBe List("a++")

        case _ => fail("No `if` or `else` node found.")
      }

    }

    "be correct for if-elseif-else statements" in {
      val cpg = code(basicBoilerplate("""
          |int a = 5;
          |if (a < 5) {
          | a++;
          |} else if (a > 5) {
          | a--;
          |} else {
          | a += 2;
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.sortBy(_.lineNumber).l) {
        case ifNode :: elseIfNode :: elseNode :: Nil =>
          ifNode.code shouldBe "if (a < 5)"
          ifNode.controlStructureType shouldBe ControlStructureTypes.IF
          inside(ifNode.condition.l) {
            case List(cndNode) =>
              cndNode.code shouldBe "a < 5"
            case _ => fail("Unexpected condition for `if` statement")
          }
          inside(ifNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(incCall) = blockNode.ast.isCall.l
              incCall.code shouldBe "a++"
              incCall.astParent shouldBe blockNode
            case _ => fail("Unexpected body contents for `if` statement")
          }

          elseIfNode.code shouldBe "if (a > 5)"
          elseIfNode.controlStructureType shouldBe ControlStructureTypes.IF
          inside(elseIfNode.condition.l) {
            case List(cndNode) =>
              cndNode.code shouldBe "a > 5"
            case _ => fail("Unexpected condition for `else if` statement")
          }
          inside(elseIfNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(decCall) = blockNode.ast.isCall.l
              decCall.code shouldBe "a--"
              decCall.astParent shouldBe blockNode
            case _ => fail("Unexpected body contents for `else if` statement")
          }

          elseNode.code shouldBe "else"
          elseNode.controlStructureType shouldBe ControlStructureTypes.ELSE
          inside(elseNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(plusEqualsCall) = blockNode.ast.isCall.l
              plusEqualsCall.code shouldBe "a += 2"
              plusEqualsCall.astParent shouldBe blockNode
            case _ => fail("Unexpected body contents for `else` statement")

          }

        case _ => fail("Incorrect number of control structures in the method.")
      }
    }
  }

}
