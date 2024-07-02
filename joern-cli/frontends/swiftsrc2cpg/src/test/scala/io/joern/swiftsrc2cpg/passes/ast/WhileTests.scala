package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class WhileTests extends AstSwiftSrc2CpgSuite {

  "WhileTests" should {

    "whileTest1" in {
      val cpg = code("""
        |while (true) {
        |  print("Endless Loop")
        |}
        |""".stripMargin)
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

    "whileTest2" in {
      val cpg = code("""
        |while (i <= n) {
        |  print(i)
        |  i = i + 1
        |}
        |""".stripMargin)
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

    "whileTest3" in {
      val cpg = code("""
        |repeat {
        |  print("Endless Loop")
        |} while (true)
        |""".stripMargin)
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

    "whileTest4" in {
      val cpg = code("""
        |repeat {
        |  print(i)
        |  i = i + 1
        |} while (i <= n)
        |""".stripMargin)
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
