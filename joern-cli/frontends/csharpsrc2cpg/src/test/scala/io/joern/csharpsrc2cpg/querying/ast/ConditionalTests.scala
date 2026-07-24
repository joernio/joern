package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{ControlStructure, Identifier, Literal, Call}
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

      inside(cpg.method("Main").controlStructure.l) { case ifNode :: Nil =>
        ifNode.code shouldBe "if (a == 1)"
        ifNode.controlStructureType shouldBe ControlStructureTypes.IF
        inside(ifNode.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "a == 1"
        }
        ifNode.whenTrue.assignment.code.l shouldBe List("a++")

        inside(ifNode.astChildren.isBlock.l) { case blockNode :: Nil =>
          val List(incCall) = blockNode.ast.isCall.l
          incCall.code shouldBe "a++"
          incCall.astParent shouldBe blockNode
        }

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
      inside(cpg.method("Main").controlStructure.l) { case ifNode :: Nil =>
        ifNode.code shouldBe "if (a == 1)"
        ifNode.controlStructureType shouldBe ControlStructureTypes.IF

        inside(ifNode.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "a == 1"
        }

        inside(ifNode.whenTrue.isBlock.l) { case blockNode :: Nil =>
          val List(incCall) = blockNode.ast.isCall.l
          incCall.code shouldBe "a++"
          incCall.astParent shouldBe blockNode
        }

        inside(ifNode.whenFalse.isBlock.l) { case blockNode :: Nil =>
          val List(decCall) = blockNode.ast.isCall.l
          decCall.code shouldBe "a--"
          decCall.astParent shouldBe blockNode
        }

        ifNode.whenTrue.assignment.code.l shouldBe List("a++")
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

      inside(cpg.method("Main").controlStructure.sortBy(_.lineNumber).l) { case ifNode :: elseIfNode :: Nil =>
        ifNode.code shouldBe "if (a < 5)"
        ifNode.controlStructureType shouldBe ControlStructureTypes.IF
        inside(ifNode.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "a < 5"
        }
        inside(ifNode.whenTrue.isBlock.l) { case blockNode :: Nil =>
          val List(incCall) = blockNode.ast.isCall.l
          incCall.code shouldBe "a++"
          incCall.astParent shouldBe blockNode
        }

        // the `else if` is wired directly as the false branch of the outer `if`
        ifNode.whenFalse.l shouldBe List(elseIfNode)

        elseIfNode.code shouldBe "if (a > 5)"
        elseIfNode.controlStructureType shouldBe ControlStructureTypes.IF
        inside(elseIfNode.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "a > 5"
        }
        inside(elseIfNode.whenTrue.isBlock.l) { case blockNode :: Nil =>
          val List(decCall) = blockNode.ast.isCall.l
          decCall.code shouldBe "a--"
          decCall.astParent shouldBe blockNode
        }

        // the final `else` block is wired directly as the false branch of the `else if`
        inside(elseIfNode.whenFalse.isBlock.l) { case blockNode :: Nil =>
          val List(plusEqualsCall) = blockNode.ast.isCall.l
          plusEqualsCall.code shouldBe "a += 2"
          plusEqualsCall.astParent shouldBe blockNode
        }
      }
    }

    "be correct for ternary conditions" in {
      val cpg = code(basicBoilerplate("""
          |var foo = 1 > 2 ? 1 : 2;
          |""".stripMargin))

      inside(cpg.call.name(Operators.assignment).argument.l) {
        case (foo: Identifier) :: (controlStructure: Call) :: Nil =>
          foo.typeFullName shouldBe "System.Int32"
      }

      inside(cpg.method("Main").call(Operators.conditional).l) { case ternaryIf :: Nil =>
        ternaryIf.code shouldBe "1 > 2 ? 1 : 2"

        inside(ternaryIf.argument.l) { case (condition: Call) :: (whenTrue: Literal) :: (whenFalse: Literal) :: Nil =>
          condition.code shouldBe "1 > 2"
          condition.typeFullName shouldBe "System.Boolean"

          whenTrue.code shouldBe "1"
          whenTrue.typeFullName shouldBe "System.Int32"

          whenFalse.code shouldBe "2"
          whenFalse.typeFullName shouldBe "System.Int32"
        }

      }

    }
  }

}
