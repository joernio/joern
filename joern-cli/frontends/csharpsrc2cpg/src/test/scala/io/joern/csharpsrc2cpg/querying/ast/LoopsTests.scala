package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.semanticcpg.language.*

class LoopsTests extends CSharpCode2CpgFixture {
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

          inside(forEachNode.astChildren.isIdentifier.l) {
            case iteratorNode :: iterableNode :: Nil =>
              iteratorNode.code shouldBe "element"
              iteratorNode.typeFullName shouldBe "System.Int32"

              iterableNode.code shouldBe "fibNumbers"
              iterableNode.typeFullName shouldBe "List<int>"
            case _ => fail("No node for iterable found in `foreach` statement")
          }

          inside(forEachNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(writeCall) = cpg.call.nameExact("Write").l
              writeCall.astParent shouldBe blockNode
            case _ => fail("Correct blockNode as child not found for `foreach` statement")
          }

        case _ => fail("No control structure node found for `foreach`.")
      }
    }
  }
}
