package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.QueryEngineStatistics.{PATH_CACHE_HITS, PATH_CACHE_MISSES}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language.{toCfgNodeMethods, toExpressionMethods}

import java.util.concurrent.Callable
import scala.collection.mutable

object DebugMode {
  var enabled = false
}

/** Callable for solving a ReachableByTask
  *
  * A Java Callable is "a task that returns a result and may throw an exception", and this is the callable for
  * calculating the result for `task`.
  *
  * @param task
  *   the data flow problem to solve
  * @param context
  *   state of the data flow engine
  */
class TaskSolver(task: ReachableByTask, context: EngineContext, sources: Set[CfgNode]) extends Callable[TaskSummary] {

  import Engine._

  /** Entry point of callable. First checks if the maximum call depth has been exceeded, in which case an empty result
    * list is returned. Otherwise, the task is solved and its results are returned.
    */
  override def call(): TaskSummary = {
    if (context.config.maxCallDepth != -1 && task.callDepth > context.config.maxCallDepth) {
      TaskSummary(task, Vector(), Vector())
    } else {
      implicit val sem: Semantics = context.semantics
      val path                    = PathElement(task.sink, task.callSiteStack) +: task.initialPath
      results(task.sink, path, task.sources, task.table, task.callSiteStack)
      // TODO why do we update the call depth here?
      val finalResults = task.table.get(task.sink).get.map { r =>
        r.copy(callDepth = task.callDepth)
      }

      val (partial, complete) = finalResults.partition(_.partial)
      val newTasks = new TaskCreator(sources).createFromResults(partial).distinctBy(t => (t.sink, t.callSiteStack))
      TaskSummary(task, complete, newTasks)
    }
  }

  /** Recursively expand the DDG backwards and return a list of all results, given by at least a source node in
    * `sourceSymbols` and the path between the source symbol and the sink.
    *
    * This method stays within the method (intra-procedural analysis) and terminates at method parameters and at output
    * arguments.
    *
    * @param path
    *   This is a path from a node to the sink. The first node of the path is expanded by this method
    *
    * @param sources
    *   This is the set of sources, i.e., nodes where traversal should end.
    *
    * @param table
    *   The result table is a cache of known results that we can re-use
    *
    * @param callSiteStack
    *   This stack holds all call sites we expanded to arrive at the generation of the current task
    */
  private def results[NodeType <: CfgNode](
    sink: CfgNode,
    path: Vector[PathElement],
    sources: Set[NodeType],
    table: ResultTable,
    callSiteStack: List[Call]
  )(implicit semantics: Semantics): Vector[ReachableByResult] = {
//    if (DebugMode.enabled) println("YYY0")

    val curNode = path.head.node
//    if (DebugMode.enabled) println("YYY1")

    /** For each parent of the current node, determined via `expandIn`, check if results are available in the result
      * table. If not, determine results recursively.
      */
    def computeResultsForParents() = {
//      if (DebugMode.enabled) println("YYY2")
      expandIn(curNode, path, callSiteStack).iterator.flatMap { parent =>
//        if (DebugMode.enabled) println("YYY3")
        val ret = createResultsFromCacheOrCompute(parent, path)
//        if (DebugMode.enabled) println("YYY4")
        ret
      }.toVector
    }

    def createResultsFromCacheOrCompute(elemToPrepend: PathElement, path: Vector[PathElement]) = {
//      if (DebugMode.enabled) println("YYY5")
      val cachedResult = table.createFromTable(elemToPrepend, path)
//      if (DebugMode.enabled) println("YYY6")
      if (cachedResult.isDefined) {
        QueryEngineStatistics.incrementBy(PATH_CACHE_HITS, 1L)
        // if (DebugMode.enabled)
//          println("YYY6a")
        cachedResult.get
      } else {
        // if (DebugMode.enabled)
//          println("YYY8")
        QueryEngineStatistics.incrementBy(PATH_CACHE_MISSES, 1L)
        val newPath = elemToPrepend +: path
        results(sink, newPath, sources, table, callSiteStack)
      }
    }

    def createPartialResultForOutputArgOrRet() = {
//      if (DebugMode.enabled) println("YYY10")
      Vector(
        ReachableByResult(
          sink,
          PathElement(path.head.node, callSiteStack, isOutputArg = true) +: path.tail,
          table,
          callSiteStack,
          partial = true
        )
      )
    }

    /** Determine results for the current node
      */
    val res = curNode match {
      // Case 1: we have reached a source => return result and continue traversing (expand into parents)
      case x if sources.contains(x.asInstanceOf[NodeType]) =>
        Vector(ReachableByResult(sink, path, table, callSiteStack)) ++ deduplicate(computeResultsForParents())
      // Case 1.5: the second node on the path is a METHOD_RETURN and its a source. This clumsy check is necessary because
      // for method returns, the derived tasks we create in TaskCreator jump immediately to the RETURN statements in
      // order to only pick up values that actually propagate via a RETURN and don't just flow to METHOD_RETURN because
      // it is the exit node.
      case _
          if path.size > 1
            && path(1).node.isInstanceOf[MethodReturn]
            && sources.contains(path(1).node.asInstanceOf[NodeType]) =>
        Vector(ReachableByResult(sink, path.drop(1), table, callSiteStack)) ++ deduplicate(computeResultsForParents())

      // Case 2: we have reached a method parameter (that isn't a source) => return partial result and stop traversing
      case _: MethodParameterIn =>
        Vector(ReachableByResult(sink, path, table, callSiteStack, partial = true))
      // Case 3: we have reached a call to an internal method without semantic (return value) and
      // this isn't the start node => return partial result and stop traversing
      case call: Call
          if isCallToInternalMethodWithoutSemantic(call)
            && !isArgOrRetOfMethodWeCameFrom(call, path) =>
//        if (DebugMode.enabled) println("YYY14")
        createPartialResultForOutputArgOrRet()

      // Case 4: we have reached an argument to an internal method without semantic (output argument) and
      // this isn't the start node nor is it the argument for the parameter we just expanded => return partial result and stop traversing
      case arg: Expression
          if path.size > 1
            && arg.inCall.toList.exists(c => isCallToInternalMethodWithoutSemantic(c))
            && !arg.inCall.headOption.exists(x => isArgOrRetOfMethodWeCameFrom(x, path)) =>
//        if (DebugMode.enabled) println("YYY15")
        createPartialResultForOutputArgOrRet()

      // All other cases: expand into parents
      case _ =>
//        if (DebugMode.enabled) println("YYY16")
        val ret = deduplicate(computeResultsForParents())
//        if (DebugMode.enabled) println("YYY17")
        ret
    }
//    if (DebugMode.enabled) println("YYY18")
    table.add(curNode, res)
    if (res.size == 4) {
      println("enabling debug mode")
      DebugMode.enabled = true
    }
    res
  }

  private def isArgOrRetOfMethodWeCameFrom(call: Call, path: Vector[PathElement]): Boolean =
    path match {
      case Vector(_, PathElement(x: MethodReturn, _, _, _, _), _*)      =>
//        if (DebugMode.enabled) println("YYY19")
        methodsForCall(call).contains(x.method)
      case Vector(_, PathElement(x: MethodParameterIn, _, _, _, _), _*) =>
//        if (DebugMode.enabled) println("YYY20")
        methodsForCall(call).contains(x.method)
      case _                                                            =>
//        if (DebugMode.enabled) println("YYY21")
        false
    }

}
