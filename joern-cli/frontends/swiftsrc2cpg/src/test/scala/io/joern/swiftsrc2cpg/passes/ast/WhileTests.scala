package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class WhileTests extends AbstractPassTest {

  "WhileTests" should {

    "whileTest1" in AstFixture("""
        |while (true) {
        |  print("Endless Loop")
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.name("<global>").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("while (true) {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.WHILE
          inside(controlStruct.condition.l) { case List(cndNode: Literal) =>
            cndNode.code shouldBe "true"
          }
          controlStruct.whenTrue.code.l shouldBe List("""print("Endless Loop")""")
          controlStruct.lineNumber shouldBe Some(2)
          controlStruct.columnNumber shouldBe Some(1)
      }
    }

    "whileTest2" in AstFixture("""
        |while (i <= n) {
        |  print(i)
        |  i = i + 1
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.name("<global>").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("while (i <= n) {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.WHILE
          inside(controlStruct.condition.l) { case List(cndNode: Call) =>
            cndNode.name shouldBe Operators.lessEqualsThan
            cndNode.code shouldBe "i <= n"
            val List(i: Identifier, n: Identifier) = cndNode.argument.isIdentifier.l
            i.name shouldBe "i"
            i.order shouldBe 1
            n.name shouldBe "n"
            n.order shouldBe 2
          }
          controlStruct.whenTrue.astChildren.code.l shouldBe List("print(i)", "i = i + 1")
          controlStruct.lineNumber shouldBe Some(2)
          controlStruct.columnNumber shouldBe Some(1)
      }
    }

    "whileTest3" in AstFixture("""
        |repeat {
        |  print("Endless Loop")
        |} while (true)
        |""".stripMargin) { cpg =>
      inside(cpg.method.name("<global>").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("repeat {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.DO
          inside(controlStruct.condition.l) { case List(cndNode: Literal) =>
            cndNode.code shouldBe "true"
          }
          controlStruct.whenTrue.code.l shouldBe List("""print("Endless Loop")""")
          controlStruct.lineNumber shouldBe Some(2)
          controlStruct.columnNumber shouldBe Some(1)
      }
    }

    "whileTest4" in AstFixture("""
        |repeat {
        |  print(i)
        |  i = i + 1
        |} while (i <= n)
        |""".stripMargin) { cpg =>
      inside(cpg.method.name("<global>").block.astChildren.isControlStructure.l) {
        case List(controlStruct: ControlStructure) =>
          controlStruct.code should startWith("repeat {")
          controlStruct.controlStructureType shouldBe ControlStructureTypes.DO
          inside(controlStruct.condition.l) { case List(cndNode: Call) =>
            cndNode.name shouldBe Operators.lessEqualsThan
            cndNode.code shouldBe "i <= n"
            val List(i: Identifier, n: Identifier) = cndNode.argument.isIdentifier.l
            i.name shouldBe "i"
            i.order shouldBe 1
            n.name shouldBe "n"
            n.order shouldBe 2
          }
          controlStruct.whenTrue.astChildren.code.l shouldBe List("print(i)", "i = i + 1")
          controlStruct.lineNumber shouldBe Some(2)
          controlStruct.columnNumber shouldBe Some(1)
      }
    }

  }

}
