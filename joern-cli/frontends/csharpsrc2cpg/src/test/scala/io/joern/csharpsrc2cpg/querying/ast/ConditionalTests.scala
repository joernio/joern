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

          inside(ifNode.astChildren.isBlock.l) { case blockNode =>
            val List(incCall) = blockNode.ast.isCall.l
            incCall.code shouldBe "a++"
          }

          inside(elseNode.astChildren.isBlock.l) { case blockNode =>
            val List(decCall) = blockNode.ast.isCall.l
            decCall.code shouldBe "a--"
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

      cpg.method("Main").controlStructure.size shouldBe 3
      cpg.method("Main").ifBlock.size shouldBe 2
      cpg.method("Main").elseBlock.size shouldBe 1

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l(1)) {
        case elseIfNode =>
          elseIfNode.code shouldBe "if (a > 5)"

          elseIfNode.controlStructureType shouldBe ControlStructureTypes.IF

          inside(elseIfNode.condition.l) { case List(cndNode) =>
            cndNode.code shouldBe "a > 5"
          }
        case _ => fail("No `else if` node found.")
      }

      val ifNode     = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).head
      val elseIfNode = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.IF).last
      val elseNode   = cpg.controlStructure.controlStructureTypeExact(ControlStructureTypes.ELSE).head

      cpg.call.codeExact("a++").astParent.l shouldBe ifNode.astChildren.isBlock.l
      cpg.call.codeExact("a--").astParent.l shouldBe elseIfNode.astChildren.isBlock.l
      cpg.call.codeExact("a += 2").astParent.l shouldBe elseNode.astChildren.isBlock.l
    }
  }

}
