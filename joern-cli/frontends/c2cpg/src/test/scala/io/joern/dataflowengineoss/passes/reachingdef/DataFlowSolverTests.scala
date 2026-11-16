package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

class DataFlowSolverTests extends DataFlowCodeToCpgSuite {

  "the forward solver on a method that ends in a loop" should {

    /** In `f`, the loop condition is the method return's first CFG predecessor and the first output parameter's only
      * flow-graph predecessor, but its successors are the loop body and the method return. The assignment in the body
      * reaches the condition only through the back edge.
      */
    val cpg = code("""
        |void f(int n) {
        |  int x = 0;
        |  for (int i = 0; i < n; i++) {
        |    x = n;
        |  }
        |}
        |""".stripMargin)

    "solve the equation at every node" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        val problem  = ReachingDefProblem.create(method)
        val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
        problem.flowGraph.allNodesReversePostOrder.foreach { node =>
          val expected = problem.flowGraph.pred(node).iterator.map(solution.out).foldLeft(mutable.BitSet())(_ | _)
          solution.in(node) shouldBe expected
        }
      }
    }

    "let the definition in the loop reach the method return" in {
      inside(cpg.call.codeExact("x = n").l) { case List(assignment) =>
        assignment.out(EdgeTypes.REACHING_DEF).collectAll[MethodReturn].l shouldBe cpg.method
          .nameExact("f")
          .methodReturn
          .l
      }
    }
  }
}
