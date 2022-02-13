package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

import scala.collection.mutable

class DataFlowSolver {

  /** Calculate fix point solution via a standard work list algorithm (Forwards). The result is given by two maps: `in`
    * and `out`. These maps associate all CFG nodes with the set of definitions at node entry and node exit
    * respectively.
    */
  def calculateMopSolutionForwards[T <: Iterable[_]](problem: DataFlowProblem[T]): Solution[T] = {
    var out: Map[StoredNode, T] = problem.inOutInit.initOut
    var in                      = problem.inOutInit.initIn
    val workList                = mutable.ListBuffer[StoredNode]()
    workList ++= problem.flowGraph.allNodesReversePostOrder

    while (workList.nonEmpty) {
      val newEntries = workList.flatMap { n =>
        val inSet = problem.flowGraph
          .pred(n)
          .map(out)
          .reduceOption((x, y) => problem.meet(x, y))
          .getOrElse(problem.empty)
        in += n -> inSet
        val old     = out(n)
        val newSet  = problem.transferFunction(n, inSet)
        val changed = !old.equals(newSet)
        out += n -> newSet
        if (changed) {
          problem.flowGraph.succ(n)
        } else
          List()
      }
      workList.clear()
      workList ++= newEntries.distinct
    }
    Solution(in, out, problem)
  }

  /** Calculate fix point solution via a standard work list algorithm (Backwards). The result is given by two maps: `in`
    * and `out`. These maps associate all CFG nodes with the set of definitions at node entry and node exit
    * respectively.
    */
  def calculateMopSolutionBackwards[T <: Iterable[_]](problem: DataFlowProblem[T]): Solution[T] = {
    var out: Map[StoredNode, T] = problem.inOutInit.initOut
    var in                      = problem.inOutInit.initIn
    val workList                = mutable.ListBuffer[StoredNode]()
    workList ++= problem.flowGraph.allNodesPostOrder

    while (workList.nonEmpty) {
      val newEntries = workList.flatMap { n =>
        val outSet = problem.flowGraph
          .succ(n)
          .map(in)
          .reduceOption((x, y) => problem.meet(x, y))
          .getOrElse(problem.empty)
        out += n -> outSet
        val old     = in(n)
        val newSet  = problem.transferFunction(n, outSet)
        val changed = !old.equals(newSet)
        in += n -> newSet
        if (changed)
          problem.flowGraph.pred(n)
        else
          List()
      }
      workList.clear()
      workList ++= newEntries.distinct
    }
    Solution(in, out, problem)
  }

}
