package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.Engine.argToOutputParams
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Expression, MethodParameterIn, MethodParameterOut}
import io.shiftleft.semanticcpg.language.NoResolve
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable

/** Creation of new tasks from results of completed tasks.
  */
class TaskCreator(sources: Set[CfgNode]) {

  /** For a given list of results and sources, generate new tasks.
    */
  def createFromResults(results: Vector[ReachableByResult]): Vector[ReachableByTask] =
    tasksForParams(results) ++ tasksForUnresolvedOutArgs(results)

  /** Determine all results that provide paths that start in a parameter. There are two cases here:
    *
    * Case 1: walking backward from the sink, we have only expanded into callers to far. In this case, the next tasks
    * need to explore each call site to the method.
    *
    * Case 2: we expanded into a callee that we identified on the way, e.g., a method `y = transform(x)`, and we have
    * reached the parameter of that method (`transform`). Upon doing so, we recorded the call site that we expanded in
    * `result.callSite`. We would now like to continue exploring from the corresponding argument at that call site only.
    */
  private def tasksForParams(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {
    startsAtParameter(results).flatMap { result =>
      val param = result.path.head.node.asInstanceOf[MethodParameterIn]
      if (result.callSiteStack.isEmpty) {
        // Case 1
        paramToArgs(param).map { arg =>
          ReachableByTask(arg, sources, new ResultTable, result.path, result.callDepth + 1)
        }
      } else {
        // Case 2
        // TODO generate new task here
        List()
      }
    }
  }

  /** Returns only those results that start at a parameter node.
    */
  private def startsAtParameter(results: Vector[ReachableByResult]) = {
    results.collect { case r: ReachableByResult if r.path.head.node.isInstanceOf[MethodParameterIn] => r }
  }


  /** For a given parameter of a method, determine all corresponding arguments at all call sites to the method.
    */
  private def paramToArgs(param: MethodParameterIn): List[Expression] =
    NoResolve
      .getMethodCallsites(param.method)
      .to(Traversal)
      .collectAll[Call]
      .argument(param.index)
      .l

  private def tasksForUnresolvedOutArgs(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {

    val outArgsAndCalls = results
      .map(x => (x, x.unresolvedArgs.collect { case e: Expression => e }, x.path, x.callDepth))
      .distinct

    val forCalls = outArgsAndCalls.flatMap { case (_, args, path, callDepth) =>
      val outCalls = args.collect { case n: Call => n }
      val methodReturns = outCalls
        .flatMap(x => NoResolve.getCalledMethods(x).methodReturn.map(y => (x, y)))
        .to(Traversal)

      methodReturns.map { case (call, ret) =>
        val newPath = Vector(path.head.copy(resolved = true)) ++ path.tail
        // TODO here, we need to add to the call site stack, not just initialize a 1-elemented stack
        val callSiteStack = mutable.Stack(call)
        ReachableByTask(ret, sources, new ResultTable, newPath, callDepth + 1, callSiteStack)
      }
    }

    val forArgs = outArgsAndCalls.flatMap { case (result, args, path, callDepth) =>
      args.flatMap { arg =>
        val outParams = if (result.callSiteStack.nonEmpty) {
          List[MethodParameterOut]()
        } else {
          argToOutputParams(arg).l
        }
        val newPath = Vector(path.head.copy(resolved = true)) ++ path.tail
        outParams
          .map { p =>
            // TODO here, we need to add to the call site stack, not just initialize a 1-elemented stack
            val callSiteStack = arg.inCall.headOption.map(x => mutable.Stack(x)).getOrElse(mutable.Stack())
            ReachableByTask(p, sources, new ResultTable, newPath, callDepth + 1, callSiteStack)
          }
      }
    }

    forCalls ++ forArgs
  }

}
