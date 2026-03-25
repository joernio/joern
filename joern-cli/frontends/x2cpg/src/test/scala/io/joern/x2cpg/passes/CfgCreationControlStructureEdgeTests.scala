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

    "prefer FOR_BODY edge over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testForBody").fullName("testForBody").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val forStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.FOR).code("for"))
      val realBodyCall  = graph.addNode(NewCall().name("realBody").code("realBody").order(1))
      val wrongBodyCall = graph.addNode(NewCall().name("wrongBody").code("wrongBody").order(4))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, forStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(forStructure, realBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, wrongBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, realBodyCall, EdgeTypes.FOR_BODY)
      }

      new CfgCreationPass(cpg).createAndApply()

      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("realBody"))
    }

    "prefer FOR_INIT edge over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testForInit").fullName("testForInit").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val forStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.FOR).code("for"))
      val wrongInitCall = graph.addNode(NewCall().name("wrongInit").code("wrongInit").order(1))
      val realInitCall  = graph.addNode(NewCall().name("realInit").code("realInit").order(3))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, forStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(forStructure, wrongInitCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, realInitCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, realInitCall, EdgeTypes.FOR_INIT)
      }

      new CfgCreationPass(cpg).createAndApply()

      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("realInit"))
    }

    "prefer FOR_UPDATE edge over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testForUpdate").fullName("testForUpdate").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val forStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.FOR).code("for"))
      val bodyCall    = graph.addNode(NewCall().name("forBody").code("forBody").order(4))
      val wrongUpdate = graph.addNode(NewCall().name("wrongUpdate").code("wrongUpdate").order(3))
      val realUpdate  = graph.addNode(NewCall().name("realUpdate").code("realUpdate").order(1))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, forStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(forStructure, bodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, wrongUpdate, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, realUpdate, EdgeTypes.AST)
        diffGraphBuilder.addEdge(forStructure, realUpdate, EdgeTypes.FOR_UPDATE)
      }

      new CfgCreationPass(cpg).createAndApply()

      bodyCall.out(EdgeTypes.CFG).cast[CfgNode].isCall.code.toList.shouldBe(List("realUpdate"))
    }

    "prefer TRY_BODY edge over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testTryBody").fullName("testTryBody").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val tryStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.TRY).code("try"))
      val wrongTryBodyBlock = graph.addNode(NewBlock().code("wrongTryBodyBlock").order(1))
      val realTryBodyBlock  = graph.addNode(NewBlock().code("realTryBodyBlock").order(2))
      val wrongTryBodyCall  = graph.addNode(NewCall().name("wrongTryBody").code("wrongTryBody").order(1))
      val realTryBodyCall   = graph.addNode(NewCall().name("realTryBody").code("realTryBody").order(1))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, tryStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(tryStructure, wrongTryBodyBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, realTryBodyBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, realTryBodyBlock, EdgeTypes.TRY_BODY)

        diffGraphBuilder.addEdge(wrongTryBodyBlock, wrongTryBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(realTryBodyBlock, realTryBodyCall, EdgeTypes.AST)
      }

      new CfgCreationPass(cpg).createAndApply()

      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("realTryBody"))
    }

    "prefer CATCH_BODY edge over legacy AST catch discovery" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testCatchBody").fullName("testCatchBody").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val tryStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.TRY).code("try"))
      val tryBodyBlock = graph.addNode(NewBlock().code("tryBodyBlock").order(1))
      val tryBodyCall  = graph.addNode(NewCall().name("tryBody").code("tryBody").order(1))

      val wrongCatch =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.CATCH).code("catch"))
      val realCatch =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.CATCH).code("catch"))
      val wrongCatchCall = graph.addNode(NewCall().name("wrongCatchCall").code("wrongCatchCall").order(1))
      val realCatchCall  = graph.addNode(NewCall().name("realCatchCall").code("realCatchCall").order(1))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, tryStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(tryStructure, tryBodyBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryBodyBlock, tryBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, wrongCatch, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, realCatch, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, tryBodyBlock, EdgeTypes.TRY_BODY)
        diffGraphBuilder.addEdge(tryStructure, realCatch, EdgeTypes.CATCH_BODY)

        diffGraphBuilder.addEdge(wrongCatch, wrongCatchCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(realCatch, realCatchCall, EdgeTypes.AST)
      }

      new CfgCreationPass(cpg).createAndApply()

      tryBodyCall.out(EdgeTypes.CFG).cast[CfgNode].isCall.code.toList.shouldBe(List("realCatchCall"))
    }

    "prefer FINALLY_BODY edge over legacy AST finally discovery" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method = graph.addNode(NewMethod().name("testFinallyBody").fullName("testFinallyBody").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val tryStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.TRY).code("try"))
      val tryBodyBlock = graph.addNode(NewBlock().code("tryBodyBlock").order(1))
      val tryBodyCall  = graph.addNode(NewCall().name("tryBody").code("tryBody").order(1))

      val wrongFinally =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.FINALLY).code("finally"))
      val realFinally =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.FINALLY).code("finally"))
      val wrongFinallyCall = graph.addNode(NewCall().name("wrongFinallyCall").code("wrongFinallyCall").order(1))
      val realFinallyCall  = graph.addNode(NewCall().name("realFinallyCall").code("realFinallyCall").order(1))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, tryStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(tryStructure, tryBodyBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryBodyBlock, tryBodyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, wrongFinally, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, realFinally, EdgeTypes.AST)
        diffGraphBuilder.addEdge(tryStructure, tryBodyBlock, EdgeTypes.TRY_BODY)
        diffGraphBuilder.addEdge(tryStructure, realFinally, EdgeTypes.FINALLY_BODY)

        diffGraphBuilder.addEdge(wrongFinally, wrongFinallyCall, EdgeTypes.AST)
        diffGraphBuilder.addEdge(realFinally, realFinallyCall, EdgeTypes.AST)
      }

      new CfgCreationPass(cpg).createAndApply()

      tryBodyCall.out(EdgeTypes.CFG).cast[CfgNode].isCall.code.toList.shouldBe(List("realFinallyCall"))
    }

    "prefer ARGUMENT edge for throw expression over legacy AST order" in {
      val cpg   = Cpg.empty
      val graph = cpg.graph

      val method       = graph.addNode(NewMethod().name("testThrowExpr").fullName("testThrowExpr").signature("void()"))
      val methodReturn = graph.addNode(NewMethodReturn().typeFullName("void").order(2))
      val methodBlock  = graph.addNode(NewBlock().code("methodBlock").order(1))

      val throwStructure =
        graph.addNode(NewControlStructure().controlStructureType(ControlStructureTypes.THROW).code("throw"))
      val wrongThrowExpr = graph.addNode(NewCall().name("wrongThrowExpr").code("wrongThrowExpr").order(1))
      val realThrowExpr  = graph.addNode(NewCall().name("realThrowExpr").code("realThrowExpr").order(2))

      graph.applyDiff { diffGraphBuilder =>
        diffGraphBuilder.addEdge(method, methodReturn, EdgeTypes.AST)
        diffGraphBuilder.addEdge(method, methodBlock, EdgeTypes.AST)
        diffGraphBuilder.addEdge(methodBlock, throwStructure, EdgeTypes.AST)

        diffGraphBuilder.addEdge(throwStructure, wrongThrowExpr, EdgeTypes.AST)
        diffGraphBuilder.addEdge(throwStructure, realThrowExpr, EdgeTypes.AST)
        diffGraphBuilder.addEdge(throwStructure, realThrowExpr, EdgeTypes.ARGUMENT)
      }

      new CfgCreationPass(cpg).createAndApply()

      method.out(EdgeTypes.CFG).cast[CfgNode].code.toList.shouldBe(List("realThrowExpr"))
    }
  }
}
