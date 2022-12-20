package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.Engine.argToOutputParams
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, MethodParameterIn, MethodParameterOut, Return}
import io.shiftleft.semanticcpg.language.NoResolve
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{NodeOps, Traversal}

/** Creation of new tasks from results of completed tasks.
  */
class TaskCreator() {

  /** For a given list of results and sources, generate new tasks.
    */
  def createFromResults(results: Vector[ReachableByResult]): Vector[ReachableByTask] =
    tasksForParams(results) ++ tasksForUnresolvedOutArgs(results)

  /** Create new tasks from all results that start in a parameter. In essence, we want to traverse to corresponding
    * arguments of call sites, but we need to be careful here not to create unrealizable paths. We achieve this by
    * holding a call stack in results.
    *
    * Case 1: we expanded into a callee that we identified on the way, e.g., a method `y = transform(x)`, and we have
    * reached the parameter of that method (`transform`). Upon doing so, we recorded the call site that we expanded in
    * `result.callSite`. We would now like to continue exploring from the corresponding argument at that call site only.
    *
    * Case 2: walking backward from the sink, we have only expanded into callers so far, that is, the call stack is
    * empty. In this case, the next tasks need to explore each call site to the method.
    */
  private def tasksForParams(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {
    startsAtParameter(results).flatMap { result =>
      val param = result.path.head.node.asInstanceOf[MethodParameterIn]
      result.callSiteStack match {
        case callSite :: tail =>
          // Case 1
          paramToArgs(param).filter(x => x.inCall.exists(c => c == callSite)).map { arg =>
            ReachableByTask(result.taskStack :+ TaskFingerprint(arg, tail, result.callDepth - 1), result.path)
          }
        case _ =>
          // Case 2
          paramToArgs(param).map { arg =>
            ReachableByTask(result.taskStack :+ TaskFingerprint(arg, List(), result.callDepth + 1), result.path)
          }
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
    paramToArgsOfCallers(param) ++ paramToMethodRefCallReceivers(param)

  private def paramToArgsOfCallers(param: MethodParameterIn): List[Expression] =
    NoResolve
      .getMethodCallsites(param.method)
      .to(Traversal)
      .collectAll[Call]
      .argument(param.index)
      .l

  /** Expand to receiver objects of calls that reference the method of the parameter, e.g., if `param` is a parameter of
    * `m`, return `foo` in `foo.bar(m)` TODO: I'm not sure whether `methodRef.methodFullNameExact(...)` uses an index.
    * If not, then caching these lookups or keeping a map of all method names to their references may make sense.
    */

  private def paramToMethodRefCallReceivers(param: MethodParameterIn): List[Expression] =
    new Cpg(param.graph()).methodRef.methodFullNameExact(param.method.fullName).inCall.argument(0).l

  /** Create new tasks from all results that end in an output argument, including return arguments. In this case, we
    * want to traverse to corresponding method output parameters and method return nodes respectively.
    */
  private def tasksForUnresolvedOutArgs(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {

    val outArgsAndCalls = results
      .map(x => (x, x.outputArgument, x.path, x.callDepth))
      .distinct

    val forCalls = outArgsAndCalls.flatMap { case (result, outArg, path, callDepth) =>
      val outCall = outArg.collect { case n: Call => n }

      val methodReturns = outCall.toList
        .flatMap(x => NoResolve.getCalledMethods(x).methodReturn.map(y => (x, y)))
        .to(Traversal)

      methodReturns.flatMap { case (call, methodReturn) =>
        val method           = methodReturn.method.head
        val returnStatements = methodReturn._reachingDefIn.toList.collect { case r: Return => r }
        if (method.isExternal || method.start.isStub.nonEmpty) {
          val newPath = path
          (call.receiver.l ++ call.argument.l).map { arg =>
            val taskStack = result.taskStack :+ TaskFingerprint(arg, result.callSiteStack, callDepth)
            ReachableByTask(taskStack, newPath)
          }
        } else {
          returnStatements.map { returnStatement =>
            val newPath = Vector(PathElement(methodReturn, result.callSiteStack)) ++ path
            val taskStack =
              result.taskStack :+ TaskFingerprint(returnStatement, call :: result.callSiteStack, callDepth + 1)
            ReachableByTask(taskStack, newPath)
          }
        }
      }
    }

    val forArgs = outArgsAndCalls.flatMap { case (result, args, path, callDepth) =>
      args.toList.flatMap { case arg: Expression =>
        val outParams = if (result.callSiteStack.nonEmpty) {
          List[MethodParameterOut]()
        } else {
          argToOutputParams(arg).l
        }
        outParams
          .filterNot(_.method.isExternal)
          .map { p =>
            val newStack = arg.inCall.headOption.map { x => x :: result.callSiteStack }.getOrElse(result.callSiteStack)
            ReachableByTask(result.taskStack :+ TaskFingerprint(p, newStack, callDepth + 1), path)
          }
      }
    }

    forCalls ++ forArgs
  }

}
