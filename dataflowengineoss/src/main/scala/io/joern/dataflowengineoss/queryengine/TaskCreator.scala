package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.queryengine.Engine.argToOutputParams
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

/** Creation of new tasks from results of completed tasks.
  */
class TaskCreator(context: EngineContext) {

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /** For a given list of results and sources, generate new tasks.
    */
  def createFromResults(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {
    val newTasks = tasksForParams(results) ++ tasksForUnresolvedOutArgs(results)
    removeTasksWithLoopsAndTooHighCallDepth(newTasks)
  }

  private def removeTasksWithLoopsAndTooHighCallDepth(tasks: Vector[ReachableByTask]): Vector[ReachableByTask] = {
    val tasksWithValidCallDepth = if (context.config.maxCallDepth == -1) {
      tasks
    } else {
      tasks.filter(_.callDepth <= context.config.maxCallDepth)
    }
    tasksWithValidCallDepth.filter { t =>
      t.taskStack.dedup.size == t.taskStack.size
    }
  }

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
  private def paramToArgs(param: MethodParameterIn): List[Expression] = {
    val args = paramToArgsOfCallers(param) ++ paramToMethodRefCallReceivers(param)
    if (args.size > context.config.maxArgsToAllow) {
      logger.warn(s"Too many arguments for parameter: ${args.size}. Not expanding")
      logger.warn("Method name: " + param.method.fullName)
      List()
    } else {
      args
    }
  }

  private def paramToArgsOfCallers(param: MethodParameterIn): List[Expression] =
    NoResolve
      .getMethodCallsites(param.method)
      .collectAll[Call]
      .argument(param.index)
      .l

  /** Expand to receiver objects of calls that reference the method of the parameter, e.g., if `param` is a parameter of
    * `m`, return `foo` in `foo.bar(m)` TODO: I'm not sure whether `methodRef.methodFullNameExact(...)` uses an index.
    * If not, then caching these lookups or keeping a map of all method names to their references may make sense.
    */
  private def paramToMethodRefCallReceivers(param: MethodParameterIn): List[Expression] = {
    val cpg  = new Cpg(param.graph)
    def trav = cpg.methodRef.methodFullNameExact(param.method.fullName).inCall
    cpg.metaData.language.headOption match {
      // Kotlin higher-level functions are often static and don't have the arg0 recv
      case Some(Languages.KOTLIN) => trav.argument(1).l
      case _                      => trav.argument(0).l
    }
  }

  /** Create new tasks from all results that end in an output argument, including return arguments. In this case, we
    * want to traverse to corresponding method output parameters and method return nodes respectively.
    */
  private def tasksForUnresolvedOutArgs(results: Vector[ReachableByResult]): Vector[ReachableByTask] = {

    val outArgsAndCalls = results
      .map(x => (x, x.outputArgument, x.path, x.callDepth))
      .distinct

    val forCalls = outArgsAndCalls.flatMap { case (result, outArg, path, callDepth) =>
      outArg.toList.flatMap {
        case call: Call =>
          val methodReturns = call.toList
            .flatMap(x => NoResolve.getCalledMethods(x).methodReturn.map(y => (x, y)))
            .iterator

          methodReturns.flatMap { case (call, methodReturn) =>
            val method           = methodReturn.method
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
        case _ => Vector.empty
      }
    }

    val forArgs = outArgsAndCalls.flatMap { case (result, args, path, callDepth) =>
      args.toList.flatMap {
        case arg: Expression =>
          val outParams = if (result.callSiteStack.nonEmpty) {
            List[MethodParameterOut]()
          } else {
            argToOutputParams(arg).l
          }
          outParams
            .filterNot(_.method.isExternal)
            .map { p =>
              val newStack =
                arg.inCall.headOption.map { x => x :: result.callSiteStack }.getOrElse(result.callSiteStack)
              ReachableByTask(result.taskStack :+ TaskFingerprint(p, newStack, callDepth + 1), path)
            }
        case _ => Vector.empty
      }
    }

    val forMethodRefs = outArgsAndCalls.flatMap { case (result, outArg, path, callDepth) =>
      outArg.toList.flatMap {
        case methodRef: MethodRef =>
          val methodReturns = methodRef._refOut.collectAll[Method].methodReturn
          methodReturns.flatMap { methodReturn =>
            val returnStatements = methodReturn._reachingDefIn.toList.collect { case r: Return => r }
            returnStatements.map { returnStatement =>
              val newPath = Vector(PathElement(methodReturn, result.callSiteStack)) ++ path
              val taskStack =
                result.taskStack :+ TaskFingerprint(returnStatement, result.callSiteStack, callDepth + 1)
              ReachableByTask(taskStack, newPath)
            }
          }
        case _ => Vector.empty
      }
    }
    restrictSize(forCalls) ++ restrictSize(forArgs) ++ restrictSize(forMethodRefs)
  }

  private def restrictSize(l: Vector[ReachableByTask]): Vector[ReachableByTask] = {
    if (l.size <= context.config.maxOutputArgsExpansion) {
      l
    } else {
      logger.warn("Too many new tasks in expansion of unresolved output arguments")
      Vector()
    }
  }

}
