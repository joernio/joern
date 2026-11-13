package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.passes.reachingdef.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess.isGenericMemberAccessName

import scala.collection.mutable

/** The transfer function removes all definitions of each variable a node defines (its kill masks) and adds `gen` back.
  * These tests compare it, and the public `kill` sets, with the kill sets as master builds them, one per call, each
  * holding the other definitions of those variables.
  */
class ReachingDefKillTests extends DataFlowCodeToCpgSuite {

  /** The kill sets as master builds them, scanning every call for field accesses on each identifier. */
  private def previousKill(transfer: ReachingDefTransferFunction, flowGraph: ReachingDefFlowGraph) = {
    val method       = flowGraph.method
    val nodeToNumber = flowGraph.nodeToNumber
    val identifiers  = (method._identifierViaContainsOut.map(identifier => identifier.name -> (identifier: CfgNode)) ++
      method.parameter.map(param => param.name -> (param: CfgNode))).l.groupMap(_._1)(_._2)
    val calls = method.call.l.groupBy(_.code).withDefaultValue(Nil)

    def others(definition: Int): Iterator[Int] = {
      val nodes = flowGraph.numberToNode(definition) match {
        case param: MethodParameterIn => identifiers(param.name).iterator.filter(_.id != param.id)
        case identifier: Identifier   =>
          identifiers(identifier.name).iterator.filter(_.id != identifier.id) ++
            calls.valuesIterator.flatten
              .filter(_.name == Operators.fieldAccess)
              .filter(_.ast.isIdentifier.nameExact(identifier.name).nonEmpty)
        case call: Call => calls(call.code).iterator.filter(_.id != call.id)
        case _          => Iterator.empty
      }
      nodes.collect { case node if nodeToNumber.contains(node) => nodeToNumber(node) }
    }

    method.call
      .filterNot(call => isGenericMemberAccessName(call.name))
      .map(call => (call: CfgNode) -> mutable.BitSet.fromSpecific(transfer.gen(call).iterator.flatMap(others)))
      .toMap
      .withDefaultValue(mutable.BitSet())
  }

  /** Checks every node of `method` on the set of all definitions and on the sets the solver reaches, and returns the
    * kill sets the comparison used.
    */
  private def compareWithPreviousKill(method: Method): Map[CfgNode, mutable.BitSet] = {
    val flowGraph = new ReachingDefFlowGraph(method)
    val transfer  = new OptimizedReachingDefTransferFunction(flowGraph)
    val kill      = previousKill(transfer, flowGraph)
    val problem   = new DataFlowProblem[CfgNode, mutable.BitSet](
      flowGraph,
      transfer,
      (left, right) => left.union(right),
      new ReachingDefInit(transfer.gen),
      true,
      mutable.BitSet()
    )
    val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val all      = mutable.BitSet.fromSpecific(flowGraph.numberToNode.keys)
    for (node <- flowGraph.numberToNode.values) {
      transfer.kill(node) shouldBe kill(node)
      for (in <- Iterator(all) ++ solution.in.get(node)) {
        transfer(node, in) shouldBe transfer.gen(node).union(in.diff(kill(node)))
      }
    }
    kill
  }

  private def killedBy(kill: Map[CfgNode, mutable.BitSet], method: Method, code: String): Set[String] = {
    val numberToNode = new ReachingDefFlowGraph(method).numberToNode
    kill(method.call.codeExact(code).head).iterator.map(numberToNode).map(node => s"${node.label}:${node.code}").toSet
  }

  "the transfer function" should {

    "kill the parameter and identifiers of a redefined identifier's name" in {
      val cpg = code("""
          |int f(int a, int b) {
          |  a = b;
          |  a = 2;
          |  return a + b;
          |}
          |""".stripMargin)
      val method = cpg.method.nameExact("f").head
      killedBy(compareWithPreviousKill(method), method, "a = 2") should contain allOf (
        "METHOD_PARAMETER_IN:int a",
        "IDENTIFIER:a"
      )
    }

    "kill the field accesses on a redefined identifier" in {
      val cpg = code("""
          |struct s { int x; };
          |int f(struct s t, struct s u) {
          |  int y = t.x;
          |  t = u;
          |  return y + t.x;
          |}
          |""".stripMargin)
      val method = cpg.method.nameExact("f").head
      killedBy(compareWithPreviousKill(method), method, "t = u") should contain("CALL:t.x")
    }

    "kill the calls with the same code as a defined call" in {
      val cpg = code("""
          |struct s { int x; };
          |int f(struct s t) {
          |  t.x = 1;
          |  t.x = 2;
          |  return t.x;
          |}
          |""".stripMargin)
      val method = cpg.method.nameExact("f").head
      killedBy(compareWithPreviousKill(method), method, "t.x = 2") should contain("CALL:t.x")
    }

    "kill nothing at an address-of, which generates a definition of its operand" in {
      val cpg = code("""
          |int g(int *p);
          |int f(int i) {
          |  i = i + 1;
          |  return g(&i) + i;
          |}
          |""".stripMargin)
      val method = cpg.method.nameExact("f").head
      killedBy(compareWithPreviousKill(method), method, "&i") shouldBe empty
    }
  }
}
