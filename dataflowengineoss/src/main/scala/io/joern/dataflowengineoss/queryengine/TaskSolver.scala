package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.QueryEngineStatistics.{PATH_CACHE_HITS, PATH_CACHE_MISSES}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Expression, MethodParameterIn, MethodReturn}
import io.shiftleft.semanticcpg.language.{toCfgNodeMethods, toExpressionMethods}

import java.util.concurrent.Callable
import scala.collection.mutable

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
class TaskSolver(task: ReachableByTask, context: EngineContext) extends Callable[Vector[ReachableByResult]] {

  import Engine._

  /** Entry point of callable. First checks if the maximum call depth has been exceeded, in which case an empty result
    * list is returned. Otherwise, the task is solved and its results are returned.
    */
  override def call(): Vector[ReachableByResult] = {
    if (context.config.maxCallDepth != -1 && task.callDepth > context.config.maxCallDepth) {
      Vector()
    } else {
      implicit val sem: Semantics = context.semantics
      val path                    = PathElement(task.sink) +: task.initialPath
      results(path, task.sources, task.table, task.callSiteStack)
      // TODO why do we update the call depth here?
      task.table.get(task.sink).get.map { r =>
        r.copy(callDepth = task.callDepth)
      }
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
    path: Vector[PathElement],
    sources: Set[NodeType],
    table: ResultTable,
    callSiteStack: mutable.Stack[Call]
  )(implicit semantics: Semantics): Vector[ReachableByResult] = {

    val curNode = path.head.node

    /** For each parent of the current node, determined via `expandIn`, check if results are available in the result
      * table. If not, determine results recursively.
      */
    def computeResultsForParents() = {
      expandIn(curNode, path).iterator.flatMap { parent =>
        createResultsFromCacheOrCompute(parent, path)
      }.toVector
    }

    def createResultsFromCacheOrCompute(elemToPrepend: PathElement, path: Vector[PathElement]) = {
      val cachedResult = table.createFromTable(elemToPrepend, path)
      if (cachedResult.isDefined) {
        QueryEngineStatistics.incrementBy(PATH_CACHE_HITS, 1L)
        cachedResult.get
      } else {
        QueryEngineStatistics.incrementBy(PATH_CACHE_MISSES, 1L)
        val newPath = elemToPrepend +: path
        results(newPath, sources, table, callSiteStack)
      }
    }

    def createPartialResultForOutputArgOrRet() = {
      Vector(
        ReachableByResult(
          PathElement(path.head.node, isOutputArg = true) +: path.tail,
          table,
          callSiteStack,
          partial = true
        )
      )
    }

    /** Determine results for the current node
      *
      * * Case 1: we have reached a source => return result. * Case 2: we have reached a method parameter (that is not a
      * source) => return a partial result * Case 3: we have reached an argument/call and the path is not empty =>
      * consider this an output argument and create a partial result
      */

    val res = curNode match {
      // Case 1: we have reached a source => return result and continue traversing
      case x if sources.contains(x.asInstanceOf[NodeType]) => {
        Vector(ReachableByResult(path, table, callSiteStack)) ++ deduplicate(computeResultsForParents())
      }
      // Case 1.5: the second node on the path is a METHOD_RETURN and its a source. This clumsy check is necessary because
      // for method returns, the derived tasks we create in TaskCreator jump immediately to the RETURN statements in
      // order to only pick up values that actually propagate via a RETURN and don't just flow to METHOD_RETURN because
      // it is the exit node.
      case _
          if path.size > 1 && path(1).node
            .isInstanceOf[MethodReturn] && sources.contains(path(1).node.asInstanceOf[NodeType]) =>
        Vector(ReachableByResult(path.drop(1), table, callSiteStack)) ++ deduplicate(computeResultsForParents())
      // Case 2: we have reached a method parameter (that isn't a source) => return partial result and stop traversing
      case _: MethodParameterIn =>
        Vector(ReachableByResult(path, table, callSiteStack, partial = true))
      // Case 3: we have reached a call to an internal method without semantic (return value) and
      // this isn't the start node => return partial result and stop traversing
      case call: Call
          if path.size > 1 && isCallToInternalMethodWithoutSemantic(call) && !isArgOrRetOfMethodWeCameFrom(
            call,
            path
          ) =>
        createPartialResultForOutputArgOrRet()
      // Case 4: we have reached an argument to an internal method without semantic (output argument) and
      // this isn't the start node nor is it the argument for the parameter we just expanded => return partial result and stop traversing
      case arg: Expression
          if path.size > 1 && arg.inCall.toList.exists(c =>
            isCallToInternalMethodWithoutSemantic(c)
          ) && !arg.inCall.headOption.exists(x => isArgOrRetOfMethodWeCameFrom(x, path)) =>
        createPartialResultForOutputArgOrRet()
      // All other cases: expand into parents
      case _ =>
        deduplicate(computeResultsForParents())
    }

    table.add(curNode, res)
    res
  }

  private def isArgOrRetOfMethodWeCameFrom(call: Call, path: Vector[PathElement]): Boolean = {
    if (path.size <= 1) {
      false
    } else {
      path(1).node match {
        case x: MethodParameterIn =>
          methodsForCall(call).contains(x.method)
        case x: MethodReturn =>
          methodsForCall(call).contains(x.method)
        case _ => false
      }
    }
  }

}
