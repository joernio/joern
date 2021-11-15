package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.Edge
import overflowdb.traversal.{NodeOps, Traversal}

import java.util.concurrent.{Callable, ExecutorCompletionService, ExecutorService, Executors}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

private case class ReachableByTask(sink: CfgNode,
                                   sources: Set[CfgNode],
                                   table: ResultTable,
                                   initialPath: Vector[PathElement] = Vector(),
                                   callDepth: Int = 0,
                                   callSite: Option[Call] = None)

class Engine(context: EngineContext) {

  import Engine._

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  private var numberOfTasksRunning: Int = 0
  private val executorService: ExecutorService = Executors.newWorkStealingPool()
  private val completionService = new ExecutorCompletionService[Vector[ReachableByResult]](executorService)

  def shutdown(): Unit = {
    executorService.shutdown()
  }

  /**
    * Determine flows from sources to sinks by analyzing backwards from sinks.
    * Returns the list of results along with the ResultTable, a cache of known
    * paths created during the analysis.
    * */
  def backwards(sinks: List[CfgNode], sources: List[CfgNode]): List[ReachableByResult] = {
    if (sources.isEmpty) {
      logger.warn("Attempting to determine flows from empty list of sources.")
    }
    if (sinks.isEmpty) {
      logger.warn("Attempting to determine flows to empty list of sinks.")
    }
    val sourcesSet = sources.toSet
    val tasks = sinks.map(sink => ReachableByTask(sink, sourcesSet, new ResultTable))
    solveTasks(tasks, sourcesSet)
  }

  private def solveTasks(tasks: List[ReachableByTask], sources: Set[CfgNode]): List[ReachableByResult] = {

    tasks.foreach(submitTask)
    var result = List[ReachableByResult]()
    while (numberOfTasksRunning > 0) {
      Try {
        completionService.take.get
      } match {
        case Success(resultsOfTask) =>
          numberOfTasksRunning -= 1
          val complete = resultsOfTask.filterNot(_.partial)
          result ++= complete
          newTasksFromResults(resultsOfTask, sources)
            .foreach(submitTask)
        case Failure(exception) =>
          numberOfTasksRunning -= 1
          logger.warn(exception.getMessage)
      }
    }
    deduplicate(result.toVector).toList
  }

  private def newTasksFromResults(resultsOfTask: Vector[ReachableByResult],
                                  sources: Set[CfgNode]): Vector[ReachableByTask] = {
    tasksForParams(resultsOfTask, sources) ++ tasksForUnresolvedOutArgs(resultsOfTask, sources)
  }

  private def submitTask(task: ReachableByTask): Unit = {
    numberOfTasksRunning += 1
    completionService.submit(new ReachableByCallable(task, context))
  }

  private def tasksForParams(resultsOfTask: Vector[ReachableByResult],
                             sources: Set[CfgNode]): Vector[ReachableByTask] = {
    val pathsFromParams = resultsOfTask.map(x => (x, x.path, x.callDepth))
    pathsFromParams.flatMap {
      case (result, path, callDepth) =>
        val param = path.head.node
        Some(param)
          .collect {
            case p: MethodParameterIn =>
              val args = if (result.callSite.isDefined) {
                paramToArgs(p).filter { arg =>
                  arg.inCall.headOption.map(_.id()).contains(result.callSite.get.id())
                }
              } else {
                paramToArgs(p)
              }
              args.map { arg =>
                ReachableByTask(arg, sources, new ResultTable, path, callDepth + 1)
              }
          }
          .getOrElse(Vector())
    }
  }

  private def tasksForUnresolvedOutArgs(resultsOfTask: Vector[ReachableByResult],
                                        sources: Set[CfgNode]): Vector[ReachableByTask] = {

    val outArgsAndCalls = resultsOfTask
      .map(x => (x.unresolvedArgs.collect { case e: Expression => e }, x.path, x.callDepth))
      .distinct

    val forCalls = outArgsAndCalls.flatMap {
      case (args, path, callDepth) =>
        val outCalls = args.collect { case n: Call => n }
        val methodReturns = outCalls
          .flatMap(x => NoResolve.getCalledMethods(x).methodReturn.map(y => (x, y)))
          .to(Traversal)

        methodReturns.map {
          case (call, ret) =>
            ReachableByTask(ret, sources, new ResultTable, path, callDepth + 1, Some(call))
        }
    }

    val forArgs = outArgsAndCalls.flatMap {
      case (args, path, callDepth) =>
        args.flatMap { arg =>
          argToOutputParams(arg)
            .map(p => ReachableByTask(p, sources, new ResultTable, path, callDepth + 1, arg.inCall.headOption))
        }
    }

    forCalls ++ forArgs
  }

}

object Engine {

  def expandIn(curNode: CfgNode, path: Vector[PathElement])(implicit semantics: Semantics): Vector[PathElement] = {
    curNode match {
      case argument: Expression =>
        val (arguments, nonArguments) = ddgInE(curNode, path)
          .filter { edge =>
            !isCallRetvalThatShouldNotPropagate(edge.outNode().asInstanceOf[StoredNode])
          }
          .partition(_.outNode().isInstanceOf[Expression])
        val elemsForArguments = arguments.flatMap { e =>
          elemForArgument(e, argument)
        }
        val elems = elemsForArguments ++ nonArguments.map(edgeToPathElement)
        elems
      case _ =>
        ddgInE(curNode, path)
          .filter { edge =>
            !isCallRetvalThatShouldNotPropagate(edge.outNode().asInstanceOf[StoredNode])
          }
          .map(edgeToPathElement)
    }
  }

  private def edgeToPathElement(e: Edge): PathElement = {
    val parentNode = e.outNode().asInstanceOf[CfgNode]
    val outLabel = Some(e.property(Properties.VARIABLE)).getOrElse("")
    PathElement(parentNode, outEdgeLabel = outLabel)
  }

  private def ddgInE(dstNode: CfgNode, path: Vector[PathElement]): Vector[Edge] = {
    dstNode
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .filter { e =>
        val outNode = e.outNode()
        outNode.isInstanceOf[CfgNode] && !outNode.isInstanceOf[Method]
      }
      .filter(e => !path.map(_.node).contains(e.outNode().asInstanceOf[CfgNode]))
      .toVector
  }

  def isCallRetvalThatShouldNotPropagate(parentNode: StoredNode)(implicit semantics: Semantics): Boolean = {
    parentNode match {
      case call: Call =>
        val sem = semantics.forMethod(call.methodFullName)
        (sem.isDefined && !(sem.get.mappings.map(_._2).contains(-1)))
      case _ =>
        false
    }
  }

  /**
    * For a given `(parentNode, curNode)` pair, determine whether to expand into
    * `parentNode`. If so, return a corresponding path element or None if
    * `parentNode` should not be followed. The Path element contains a Boolean
    * field to specify whether it should be visible in the flow or not, a decision
    * that can also only be made by looking at both the parent and the child.
    * */
  private def elemForArgument(e: Edge, curNode: Expression)(implicit semantics: Semantics): Option[PathElement] = {
    val parentNode = e.outNode().asInstanceOf[Expression]
    val parentNodeCall = parentNode.inCall.l
    val sameCallSite = parentNode.inCall.l == curNode.start.inCall.l

    if (sameCallSite && parentNode.isUsed && curNode.isDefined ||
        !sameCallSite && curNode.isUsed) {

      val visible = if (sameCallSite) {
        val semanticExists = parentNode.semanticsForCallByArg.nonEmpty
        val internalMethodsForCall = parentNodeCall.flatMap(methodsForCall).to(Traversal).internal
        (semanticExists && parentNode.isDefined) || internalMethodsForCall.isEmpty
      } else {
        parentNode.isDefined
      }

      Some(PathElement(parentNode, visible, outEdgeLabel = Some(e.property(Properties.VARIABLE)).getOrElse("")))
    } else {
      None
    }
  }

  def argToMethods(arg: Expression): List[Method] = {
    arg.inCall.l.flatMap { call =>
      methodsForCall(call)
    }
  }

  def argToOutputParams(arg: Expression): Traversal[MethodParameterOut] = {
    argToMethods(arg)
      .to(Traversal)
      .parameter
      .asOutput
      .order(arg.order)
  }

  def methodsForCall(call: Call): List[Method] = {
    NoResolve.getCalledMethods(call).toList
  }

  def paramToArgs(param: MethodParameterIn): List[Expression] =
    NoResolve
      .getMethodCallsites(param.method)
      .to(Traversal)
      .collectAll[Call]
      .argument(param.order)
      .l

  def deduplicate(vec: Vector[ReachableByResult]): Vector[ReachableByResult] = {
    vec
      .groupBy { x =>
        (x.path.headOption ++ x.path.lastOption, x.partial, x.callDepth)
      }
      .map {
        case (_, list) =>
          val lenIdPathPairs = list.map(x => (x.path.length, x)).toList
          val withMaxLength = (lenIdPathPairs.sortBy(_._1).reverse match {
            case Nil    => Nil
            case h :: t => h :: t.takeWhile(y => y._1 == h._1)
          }).map(_._2)

          if (withMaxLength.length == 1) {
            withMaxLength.head
          } else {
            withMaxLength.minBy { x =>
              x.path.map(_.node.id()).mkString("-")
            }
          }
      }
      .toVector
  }

}

case class EngineContext(semantics: Semantics, config: EngineConfig = EngineConfig())
case class EngineConfig(var maxCallDepth: Int = 2)

/**
  * Callable for solving a ReachableByTask
  *
  * A Java Callable is "a task that returns a result and may throw an exception", and this
  * is the callable for calculating the result for `task`.
  *
  * @param task the data flow problem to solve
  * @param context state of the data flow engine
  * */
private class ReachableByCallable(task: ReachableByTask, context: EngineContext)
    extends Callable[Vector[ReachableByResult]] {

  import Engine._

  /**
    * Entry point of callable.
    * */
  override def call(): Vector[ReachableByResult] = {
    if (task.callDepth > context.config.maxCallDepth) {
      Vector()
    } else {
      implicit val sem: Semantics = context.semantics
      results(PathElement(task.sink) +: task.initialPath, task.sources, task.table, task.callSite)
      task.table.get(task.sink).get.map { r =>
        r.copy(callDepth = task.callDepth)
      }
    }
  }

  /**
    * Recursively expand the DDG backwards and return a list of all
    * results, given by at least a source node in `sourceSymbols` and the
    * path between the source symbol and the sink.
    *
    * This method stays within the method (intra-procedural analysis) but
    * call sites which should be resolved are marked as such in the
    * ResultTable.
    *
    * @param path This is a path from a node to the sink. The first node
    *             of the path is expanded by this method
    * */
  private def results[NodeType <: CfgNode](
      path: Vector[PathElement],
      sources: Set[NodeType],
      table: ResultTable,
      callSite: Option[Call])(implicit semantics: Semantics): Vector[ReachableByResult] = {
    val curNode = path.head.node

    val resultsForParents: Vector[ReachableByResult] = {
      expandIn(curNode, path).iterator.flatMap { parent =>
        val cachedResult = table.createFromTable(parent, path)
        if (cachedResult.isDefined) {
          cachedResult.get
        } else {
          results(parent +: path, sources, table, callSite)
        }
      }.toVector
    }

    val resultsForCurNode = {
      val endStates = if (sources.contains(curNode.asInstanceOf[NodeType])) {
        List(ReachableByResult(path, table, callSite))
      } else if ((task.callDepth != context.config.maxCallDepth) && curNode.isInstanceOf[MethodParameterIn]) {
        List(ReachableByResult(path, table, callSite, partial = true))
      } else {
        List()
      }

      val retsToResolve = curNode match {
        case call: Call =>
          if ((task.callDepth != context.config.maxCallDepth) && methodsForCall(call)
                .to(Traversal)
                .internal
                .nonEmpty && semanticsForCall(call).isEmpty) {
            List(
              ReachableByResult(PathElement(path.head.node, resolved = false) +: path.tail,
                                table,
                                callSite,
                                partial = true))
          } else {
            List()
          }
        case _ => List()
      }
      endStates ++ retsToResolve
    }

    val res = deduplicate(resultsForParents ++ resultsForCurNode)
    table.add(curNode, res)
    res
  }

  private def semanticsForCall(call: Call)(implicit semantics: Semantics): List[FlowSemantic] = {
    Engine.methodsForCall(call).flatMap { method =>
      semantics.forMethod(method.fullName)
    }
  }

}
