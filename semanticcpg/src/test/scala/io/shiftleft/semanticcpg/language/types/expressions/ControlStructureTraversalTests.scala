package io.shiftleft.semanticcpg.language.types.expressions

import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewBlock, NewControlStructure, NewLiteral}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ControlStructureTraversalTests extends AnyWordSpec with Matchers {

  "ControlStructureTraversal" should {
    "prefer TRUE_BODY/FALSE_BODY edges over AST child order" in {
      val cpg = MockCpg()
        .withMethod("f")
        .withCustom { (graph, cpg) =>
          val method           = cpg.method.nameExact("f").head
          val methodBlock      = method.block
          val controlStructure = NewControlStructure().controlStructureType(ControlStructureTypes.IF).order(1)
          val conditionNode    = NewLiteral().code("cond").order(1)
          // Intentionally inverted wrt legacy order semantics: order(2) carries "legacy-true"
          // and order(3) carries "legacy-false" so the test proves TRUE_BODY/FALSE_BODY are preferred.
          val falseBodyNode = NewBlock().code("legacy-true").order(2)
          val trueBodyNode  = NewBlock().code("legacy-false").order(3)

          graph.addNode(controlStructure)
          graph.addNode(conditionNode)
          graph.addNode(falseBodyNode)
          graph.addNode(trueBodyNode)

          graph.addEdge(methodBlock, controlStructure, EdgeTypes.AST)

          graph.addEdge(controlStructure, conditionNode, EdgeTypes.AST)
          graph.addEdge(controlStructure, falseBodyNode, EdgeTypes.AST)
          graph.addEdge(controlStructure, trueBodyNode, EdgeTypes.AST)
          graph.addEdge(controlStructure, conditionNode, EdgeTypes.CONDITION)

          graph.addEdge(controlStructure, trueBodyNode, EdgeTypes.TRUE_BODY)
          graph.addEdge(controlStructure, falseBodyNode, EdgeTypes.FALSE_BODY)
        }
        .cpg

      val List(controlStructure) = cpg.controlStructure.controlStructureType(ControlStructureTypes.IF).l
      // values must follow TRUE_BODY/FALSE_BODY edges, not legacy order(2/3).
      controlStructure.whenTrue.code.l shouldBe List("legacy-false")
      controlStructure.whenFalse.code.l shouldBe List("legacy-true")
    }
  }
}
