package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.Engine.argToOutputParams
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

/** Creation of new tasks from results of completed tasks.
  */
class TaskCreator(sources: Set[CfgNode]) {

  /** For a given Vector of results, check resultType of each result and call the corresponding task creator
    */
  def createFromPartialResults(results: Vector[ReachableByResult]): Vector[ReachableByTask] =
    results.flatMap {
      case r if r.resultType == ReachParameterIn => tasksForParameterIn(r)
      case r if r.resultType == ReachCall        => tasksForCall(r)
      case r if r.resultType == ReachArgument    => tasksForArgument(r)
    }

  /** Create new tasks for a result that ends in a MethodParameterIn. In essence, we want to traverse to corresponding
    * arguments of call sites, but we need to be careful here not to create unrealizable paths. We achieve this by
    * holding a call stack in results.
    *
    * Case 1: walking backward from the sink, we have only expanded into callers so far, that is, the call stack is
    * empty. In this case, the next tasks need to explore each call site to the method.
    *
    * Case 2: we expanded into a callee that we identified on the way, e.g., a method `y = transform(x)`, and we have
    * reached the parameter of that method (`transform`). Upon doing so, we recorded the call site that we expanded in
    * `result.callSite`. We would now like to continue exploring from the corresponding argument at that call site only.
    */
  private def tasksForParameterIn(result: ReachableByResult): Vector[ReachableByTask] = {
    val ReachableByResult(path @ Vector(PathElement(param: MethodParameterIn, _, _), _*), _, callSitesStack, _, depth) =
      result
    if (callSitesStack.isEmpty) {
      // Case 1
      paramToArgs(param).map(ReachableByTask(_, sources, new ResultTable, path, depth + 1))
    } else {
      // Case 2
      val newCallSiteStack = callSitesStack.clone()
      val callSite         = newCallSiteStack.pop()
      paramToArgs(param)
        .filter(x => x.inCall.exists(c => c == callSite))
        .map(ReachableByTask(_, sources, new ResultTable, path, depth - 1, newCallSiteStack))
    }
  }

  /** For a given parameter of a method, determine all corresponding arguments at all call sites to the method.
    */
  private def paramToArgs(param: MethodParameterIn): Vector[Expression] =
    paramToArgsOfCallers(param) ++ paramToMethodRefCallReceivers(param)

  private def paramToArgsOfCallers(param: MethodParameterIn): Vector[Expression] =
    NoResolve
      .getMethodCallsites(param.method)
      .to(Traversal)
      .collectAll[Call]
      .argument(param.index)
      .toVector

  /** Expand to receiver objects of calls that reference the method of the parameter, e.g., if `param` is a parameter of
    * `m`, return `foo` in `foo.bar(m)` TODO: I'm not sure whether `methodRef.methodFullNameExact(...)` uses an index.
    * If not, then caching these lookups or keeping a map of all method names to their references may make sense.
    */

  private def paramToMethodRefCallReceivers(param: MethodParameterIn): Vector[Expression] =
    new Cpg(param.graph()).methodRef.methodFullNameExact(param.method.fullName).inCall.argument(0).toVector

  // Create new tasks for a result that ends in an CALL
  // In this case, we traverse to corresponding MethodReturn nodes
  private def tasksForCall(result: ReachableByResult): Vector[ReachableByTask] = {
    val ReachableByResult(path @ Vector(PathElement(call: Call, _, _), _*), _, stack, _, depth) = result
    NoResolve
      .getCalledMethods(call)
      .methodReturn
      .toVector
      .flatMap(methodReturn => {
        // Return and MethodReturn is Different
        val returns = methodReturn._returnViaReachingDefIn
        if (returns.isEmpty) {
          val newCallSiteStack = stack.clone().push(call)
          ReachableByTask(methodReturn, sources, new ResultTable, path, depth + 1, newCallSiteStack) :: Nil
        } else
          returns.map(ret => {
            val newPath          = PathElement(methodReturn) +: path
            val newCallSiteStack = stack.clone().push(call)
            ReachableByTask(ret, sources, new ResultTable, newPath, depth + 1, newCallSiteStack)
          })
      })
  }

  // Create new tasks from a result that ends in an output argument
  // In this case, we want to traverse to corresponding method output parameters.
  private def tasksForArgument(result: ReachableByResult): Vector[ReachableByTask] = {
    val ReachableByResult(path @ Vector(PathElement(argument: Expression, _, _), _*), _, stack, _, depth) = result
    val outParams =
      if (stack.nonEmpty)
        Vector[MethodParameterOut]()
      else
        argToOutputParams(argument).toVector

    outParams
      .filterNot(_.method.isExternal)
      .map { parameterOut =>
        val newCallSiteStack = stack.clone()
        argument.inCall.headOption.foreach(x => newCallSiteStack.push(x))
        ReachableByTask(parameterOut, sources, new ResultTable, path, depth + 1, newCallSiteStack)
      }

  }

}
