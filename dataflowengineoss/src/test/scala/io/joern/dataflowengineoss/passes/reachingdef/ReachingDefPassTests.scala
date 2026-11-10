package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.dataflowengineoss.DefaultSemantics
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Method, NewIdentifier}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class ReachingDefPassTests extends AnyWordSpec with Matchers {

  /** `big` has five definitions: its parameter, its two calls, and the identifier `x` that each call is passed. `small`
    * has two: its parameter and its call (`y` is passed only once, so the lone-identifier optimization leaves it out of
    * `gen`). Two edges a frontend creates are added to what `MockCpg` builds: the AST edge from a method to its output
    * parameter, which the flow graph follows, and the call arguments, without the `REF` edge to a type declaration that
    * `MockCpg.withIdentifierArgument` adds (an identifier refers to a local or a parameter).
    */
  private def cpgWithTwoMethods(): Cpg =
    MockCpg()
      .withMetaData()
      .withMethod("big")
      .withCallInMethod("big", "first")
      .withCallInMethod("big", "second")
      .withMethod("small")
      .withCallInMethod("small", "third")
      .withCustom { (diffGraph, cpg) =>
        cpg.method.foreach { method =>
          method.parameter.asOutput.foreach(parameterOut => diffGraph.addEdge(method, parameterOut, EdgeTypes.AST))
        }
        List("first" -> "x", "second" -> "x", "third" -> "y").foreach { case (callName, name) =>
          val call       = cpg.call.nameExact(callName).head
          val identifier = NewIdentifier().name(name).code(name).argumentIndex(1).order(1)
          diffGraph.addNode(identifier)
          diffGraph.addEdge(call, identifier, EdgeTypes.AST)
          diffGraph.addEdge(call, identifier, EdgeTypes.ARGUMENT)
          diffGraph.addEdge(call.method, identifier, EdgeTypes.CONTAINS)
        }
      }
      .cpg

  /** A [[ReachingDefPass]] that records the methods whose `gen` and kill sets it builds. Its problems are built as
    * [[ReachingDefProblem.create]] builds them, with a transfer function that records each call of `initGen` and
    * `initKill`.
    */
  private class KillSetRecordingPass(cpg: Cpg, maxNumberOfDefinitions: Int)
      extends ReachingDefPass(cpg, maxNumberOfDefinitions)(DefaultSemantics()) {

    val methodsWithGen: mutable.Set[String]      = ConcurrentHashMap.newKeySet[String]().asScala
    val methodsWithKillSets: mutable.Set[String] = ConcurrentHashMap.newKeySet[String]().asScala

    override private[reachingdef] def createProblem(method: Method): DataFlowProblem[CfgNode, mutable.BitSet] = {
      val flowGraph = new ReachingDefFlowGraph(method)
      val transfer  = new OptimizedReachingDefTransferFunction(flowGraph) {
        override def initGen(method: Method): Map[CfgNode, mutable.BitSet] = {
          methodsWithGen.add(method.fullName)
          super.initGen(method)
        }
        override private[reachingdef] def initKill(
          method: Method,
          gen: Map[CfgNode, mutable.BitSet]
        ): Map[CfgNode, mutable.BitSet] = {
          methodsWithKillSets.add(method.fullName)
          super.initKill(method, gen)
        }
      }
      ReachingDefProblem.create(flowGraph, transfer)
    }
  }

  private def reachingDefEdgeCount(method: Method): Int = {
    val nodes = Iterator(method) ++ method.parameter ++ method._containsOut ++ Iterator(method.methodReturn)
    nodes.outE(EdgeTypes.REACHING_DEF).size
  }

  "ReachingDefPass with a limit of four definitions" should {

    "skip a method over the limit without building its gen or kill sets" in {
      val pass = new KillSetRecordingPass(cpgWithTwoMethods(), maxNumberOfDefinitions = 4)
      pass.createAndApply()
      pass.methodsWithGen.toSet shouldBe Set("small")
      pass.methodsWithKillSets.toSet shouldBe Set("small")
    }

    "count each method's definitions as its gen sets hold them" in {
      val cpg = cpgWithTwoMethods()
      cpg.method.nameExact("big", "small").foreach { method =>
        val transfer = new OptimizedReachingDefTransferFunction(new ReachingDefFlowGraph(method))
        transfer.numberOfDefinitions shouldBe transfer.gen.valuesIterator.map(_.size).sum
      }
      cpg.method
        .nameExact("big", "small")
        .map { method =>
          method.name -> new OptimizedReachingDefTransferFunction(new ReachingDefFlowGraph(method)).numberOfDefinitions
        }
        .toMap shouldBe Map("big" -> 5, "small" -> 2)
    }

    "add reaching-definition edges to the method within the limit only" in {
      val cpg = cpgWithTwoMethods()
      new ReachingDefPass(cpg, maxNumberOfDefinitions = 4)(DefaultSemantics()).createAndApply()
      reachingDefEdgeCount(cpg.method.nameExact("big").head) shouldBe 0
      reachingDefEdgeCount(cpg.method.nameExact("small").head) should be > 0
    }
  }
}
