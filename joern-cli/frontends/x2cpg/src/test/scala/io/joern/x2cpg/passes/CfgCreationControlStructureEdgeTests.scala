package io.joern.x2cpg.passes

import io.joern.x2cpg.passes.controlflow.CfgCreationPass
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewLiteral,
  NewMethod,
  NewMethodReturn
}
import flatgraph.misc.TestUtils.*
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CfgCreationControlStructureEdgeTests extends AnyWordSpec with Matchers {

  "CfgCreationPass" should {
    "prefer DO_BODY edge over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testDo").fullName("testDo").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val doStructure = graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.DO).code("do"))
      // Intentionally inverted wrt legacy do-while order semantics: condition is order(1)
      // and body call is order(2), so this only works if DO_BODY is preferred over ORDER.
      val condition  = graph.addNode(NewLiteral().code("cond").order(1))
      val doBodyCall = graph.addNode(NewCall().name("bodyCall").code("bodyCall").order(2))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, doStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(doStructure, condition, EdgeTypes.AST)
        diffGraphBuilder.addEdge(doStructure, doBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(doStructure, condition, EdgeTypes.CONDITION)
        diffGraphBuilder.addEdge(doStructure, doBodyCall, EdgeTypes.DO_BODY)
      }

      new CfgCreationPass(cpg).createAndApply()

      // CFG must enter loop body via DO_BODY edge despite reversed legacy order.
      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("bodyCall"))
    }

    "fallback to legacy AST order for do-while body when DO_BODY edge is absent" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method = graph.addNode(NewMethod().name("testDoFallback").fullName("testDoFallback").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val doStructure = graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.DO).code("do"))
      val doBodyCall  = graph.addNode(NewCall().name("bodyCall").code("bodyCall").order(1))
      val condition   = graph.addNode(NewLiteral().code("cond").order(2))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, doStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(doStructure, doBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(doStructure, condition, EdgeTypes.AST)
        diffGraphBuilder.addEdge(doStructure, condition, EdgeTypes.CONDITION)
      }

      new CfgCreationPass(cpg).createAndApply()

      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("bodyCall"))
    }
  }
}
