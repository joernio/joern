package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class JumpStatementsTests extends CSharpCode2CpgFixture {
  "AST creation for jump statements" should {
    "be correct for break" in {
      val cpg = code(basicBoilerplate("""
          |for (int i = 0; i < 10; i++) {
          | break;
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.BREAK).l) {
        case breakStatement :: Nil =>
          breakStatement.controlStructureType shouldBe ControlStructureTypes.BREAK
          breakStatement.code shouldBe "break;"
          breakStatement.lineNumber shouldBe Some(10)
          breakStatement.columnNumber shouldBe Some(1)

          breakStatement.astParent shouldBe
            cpg
              .method("Main")
              .controlStructure
              .controlStructureTypeExact(ControlStructureTypes.FOR)
              .astChildren
              .isBlock
              .l
              .head
        case _ => fail("No break statement found.")
      }
    }

    "be correct for continue" in {
      val cpg = code(basicBoilerplate("""
          |int i = 0;
          |while (i < 10) {
          | if (i % 2) {
          |   continue;
          | }
          |}
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.CONTINUE).l) {
        case continueStatement :: Nil =>
          continueStatement.controlStructureType shouldBe ControlStructureTypes.CONTINUE
          continueStatement.code shouldBe "continue;"
          continueStatement.lineNumber shouldBe Some(12)
          continueStatement.columnNumber shouldBe Some(3)

          continueStatement.astParent shouldBe
            cpg
              .method("Main")
              .controlStructure
              .controlStructureTypeExact(ControlStructureTypes.IF)
              .astChildren
              .isBlock
              .l
              .head
        case _ => fail("No continue statement found.")
      }
    }

    "be correct for goto" in {
      val cpg = code(basicBoilerplate("""
          |int i = 0;
          |while (i < 10) {
          | if (i % 2) {
          |   goto End;
          | }
          |}
          |End:
          | Console.WriteLine("End");
          |""".stripMargin))

      inside(cpg.method("Main").controlStructure.controlStructureTypeExact(ControlStructureTypes.GOTO).l) {
        case gotoStatement :: Nil =>
          gotoStatement.controlStructureType shouldBe ControlStructureTypes.GOTO
          gotoStatement.code shouldBe "goto End;"
          gotoStatement.lineNumber shouldBe Some(12)
          gotoStatement.columnNumber shouldBe Some(3)

          gotoStatement.astParent shouldBe
            cpg
              .method("Main")
              .controlStructure
              .controlStructureTypeExact(ControlStructureTypes.IF)
              .astChildren
              .isBlock
              .l
              .head
        case _ => fail("No goto statement found.")
      }
    }

    "be correct for return" in {
      val cpg = code(basicBoilerplate("""
          |if (args.length == 0) {
          | return 0;
          |}
          |""".stripMargin))

      inside(cpg.method("Main").ast.isReturn.l) {
        case returnStatement :: Nil =>
          returnStatement.code shouldBe "return 0;"
          returnStatement.lineNumber shouldBe Some(10)
          returnStatement.columnNumber shouldBe Some(1)

          returnStatement.astParent shouldBe
            cpg
              .method("Main")
              .controlStructure
              .controlStructureTypeExact(ControlStructureTypes.IF)
              .astChildren
              .isBlock
              .l
              .head
        case _ => fail("No return statement found.")
      }
    }
  }
}
