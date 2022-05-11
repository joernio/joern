package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.Edge
import overflowdb.traversal.{NodeOps, Traversal}

import java.util.concurrent._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

case class ReachableByTask(
  sink: CfgNode,
  sources: Set[CfgNode],
  table: ResultTable,
  initialPath: Vector[PathElement] = Vector(),
  callDepth: Int = 0,
  callSiteStack: mutable.Stack[Call] = mutable.Stack()
)

/** The data flow engine allows determining paths to a set of sinks from a set of sources. To this end, it solves tasks
  * in parallel, creating and submitting new tasks upon completion of tasks. This class deals only with task scheduling,
  * while the creation of new tasks from existing tasks is handled by the class `TaskCreator`.
  */
class Engine(context: EngineContext) {

  import Engine._

  private val logger: Logger                   = LoggerFactory.getLogger(this.getClass)
  private var numberOfTasksRunning: Int        = 0
  private val executorService: ExecutorService = Executors.newWorkStealingPool()
  private val completionService = new ExecutorCompletionService[Vector[ReachableByResult]](executorService)

  def shutdown(): Unit = {
    executorService.shutdown()
  }

  /** Determine flows from sources to sinks by exploring the graph backwards from sinks to sources. Returns the list of
    * results along with a ResultTable, a cache of known paths created during the analysis.
    */
  def backwards(sinks: List[CfgNode], sources: List[CfgNode]): List[ReachableByResult] = {
    if (sources.isEmpty) {
      logger.info("Attempting to determine flows from empty list of sources.")
    }
    if (sinks.isEmpty) {
      logger.info("Attempting to determine flows to empty list of sinks.")
    }
    val sourcesSet = sources.toSet
    val tasks      = createOneTaskPerSink(sourcesSet, sinks)
    solveTasks(tasks, sourcesSet)
  }

  /** Create one task per sink where each task has its own result table.
    */
  private def createOneTaskPerSink(sourcesSet: Set[CfgNode], sinks: List[CfgNode]) = {

    /** Create a new result table. If `context.config.initialTable` is set, this initial table is cloned and returned.
      */
    def newResultTable() =
      context.config.initialTable.map(x => new ResultTable(x.table.clone)).getOrElse(new ResultTable)
    sinks.map(sink => ReachableByTask(sink, sourcesSet, newResultTable()))
  }

  /** Submit tasks to a worker pool, solving them in parallel. Upon receiving results for a task, new tasks are
    * submitted accordingly. Once no more tasks can be created, the list of results is returned.
    */
  private def solveTasks(tasks: List[ReachableByTask], sources: Set[CfgNode]): List[ReachableByResult] = {
    var result = List[ReachableByResult]()

    /** For a list of results, determine partial and complete results. Store complete results and derive and submit
      * tasks from partial results.
      */
    def handleResultsOfTask(resultsOfTask: Vector[ReachableByResult]): Unit = {
      val (partial, complete) = resultsOfTask.partition(_.partial)
      result ++= complete
      val newTasks = new TaskCreator(sources).createFromResults(partial)
      newTasks.foreach(submitTask)
    }

    def runUntilAllTasksAreSolved(): Unit = {
      while (numberOfTasksRunning > 0) {
        Try {
          completionService.take.get
        } match {
          case Success(resultsOfTask) =>
            numberOfTasksRunning -= 1
            handleResultsOfTask(resultsOfTask)
          case Failure(exception) =>
            numberOfTasksRunning -= 1
            logger.warn(s"SolveTask failed with exception:", exception)
            exception.printStackTrace()
        }
      }
    }

    tasks.foreach(submitTask)
    runUntilAllTasksAreSolved()
    deduplicate(result.toVector).toList
  }

  private def submitTask(task: ReachableByTask): Unit = {
    numberOfTasksRunning += 1
    completionService.submit(
      new TaskSolver(if (context.config.shareCacheBetweenTasks) task else task.copy(table = new ResultTable), context)
    )
  }

}

object Engine {

  /** Traverse from a node to incoming DDG nodes, taking into account semantics. This method is exposed via the `ddgIn`
    * step, but is also called by the engine internally by the `TaskSolver`.
    *
    * @param curNode
    *   the node to expand
    * @param path
    *   the path that has been expanded to reach the `curNode`
    */
  def expandIn(curNode: CfgNode, path: Vector[PathElement])(implicit semantics: Semantics): Vector[PathElement] = {
    curNode match {
      case argument: Expression =>
        val (arguments, nonArguments) = ddgInE(curNode, path).partition(_.outNode().isInstanceOf[Expression])
        val elemsForArguments = arguments.flatMap { e =>
          elemForArgument(e, argument)
        }
        val elems = elemsForArguments ++ nonArguments.flatMap(edgeToPathElement)
        elems
      case _ =>
        ddgInE(curNode, path).flatMap(edgeToPathElement)
    }
  }

  private def isOutputArgOfInternalMethod(arg: Expression)(implicit semantics: Semantics): Boolean = {
    arg.inCall.l match {
      case List(call) =>
        methodsForCall(call)
          .to(Traversal)
          .internal
          .isNotStub
          .nonEmpty && semanticsForCall(call).isEmpty
      case _ =>
        false
    }
  }

  /** Convert an edge to a path element. This function may return `None` if the edge is found to lead to an argument
    * that isn't used, according to semantics. It may also return `None` if the child node is an output argument of an
    * internal function.
    */
  private def edgeToPathElement(e: Edge)(implicit semantics: Semantics): Option[PathElement] = {
    val parentNode = e.outNode().asInstanceOf[CfgNode]
    val childNode  = e.inNode().asInstanceOf[CfgNode]
    val outLabel   = Some(e.property(Properties.VARIABLE)).getOrElse("")

    childNode match {
      case exp: Expression if !exp.isUsed =>
        None
      case _ =>
        Some(PathElement(parentNode, outEdgeLabel = outLabel))
    }
  }

  /** For a given node `node`, return all incoming reaching definition edges, unless the source node is (a) a METHOD
    * node, (b) already present on `path`, or (c) a CALL node to a method where the semantic indicates that taint is
    * propagated to it.
    */
  private def ddgInE(node: CfgNode, path: Vector[PathElement])(implicit semantics: Semantics): Vector[Edge] = {
    node
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .filter { e =>
        e.outNode() match {
          case srcNode: CfgNode =>
            !srcNode.isInstanceOf[Method] && !path.map(_.node).contains(srcNode) && !isCallRetval(srcNode)
          case _ => false
        }
      }
      .toVector
  }

  private def isCallRetval(node: StoredNode)(implicit semantics: Semantics): Boolean = {
    node match {
      case call: Call =>
        val sem = semantics.forMethod(call.methodFullName)
        sem.isDefined && !sem.get.mappings.map(_._2).contains(-1)
      case _ =>
        false
    }
  }

  /** For a given `(parentNode, curNode)` pair, determine whether to expand into `parentNode`. If so, return a
    * corresponding path element or None if `parentNode` should not be followed. The Path element contains a Boolean
    * field to specify whether it should be visible in the flow or not, a decision that can also only be made by looking
    * at both the parent and the child.
    */
  private def elemForArgument(e: Edge, curNode: Expression)(implicit semantics: Semantics): Option[PathElement] = {

    val parentNode     = e.outNode().asInstanceOf[Expression]
    val parentNodeCall = parentNode.inCall.l
    val sameCallSite   = parentNode.inCall.l == curNode.start.inCall.l

    if (sameCallSite && isOutputArgOfInternalMethod(parentNode)) {
      return None
    }

    if (
      sameCallSite && parentNode.isUsed && curNode.isDefined ||
      !sameCallSite && curNode.isUsed
    ) {

      val visible = if (sameCallSite) {
        val semanticExists         = parentNode.semanticsForCallByArg.nonEmpty
        val internalMethodsForCall = parentNodeCall.flatMap(methodsForCall).to(Traversal).internal
        (semanticExists && parentNode.isDefined) || internalMethodsForCall.isEmpty
      } else {
        parentNode.isDefined
      }
      val isOutputArg = isOutputArgOfInternalMethod(parentNode)
      Some(
        PathElement(
          parentNode,
          visible,
          isOutputArg,
          outEdgeLabel = Some(e.property(Properties.VARIABLE)).getOrElse("")
        )
      )
    } else {
      None
    }
  }

  def argToOutputParams(arg: Expression): Traversal[MethodParameterOut] = {
    argToMethods(arg)
      .to(Traversal)
      .parameter
      .index(arg.argumentIndex)
      .asOutput
  }

  def argToMethods(arg: Expression): List[Method] = {
    arg.inCall.l.flatMap { call =>
      methodsForCall(call)
    }
  }

  def methodsForCall(call: Call): List[Method] = {
    NoResolve.getCalledMethods(call).toList
  }

  def isCallToInternalMethod(call: Call): Boolean = {
    methodsForCall(call)
      .to(Traversal)
      .internal
      .nonEmpty
  }
  def isCallToInternalMethodWithoutSemantic(call: Call)(implicit semantics: Semantics): Boolean = {
    isCallToInternalMethod(call) && semanticsForCall(call).isEmpty
  }

  def semanticsForCall(call: Call)(implicit semantics: Semantics): List[FlowSemantic] = {
    Engine.methodsForCall(call).flatMap { method =>
      semantics.forMethod(method.fullName)
    }
  }

  def deduplicate(vec: Vector[ReachableByResult]): Vector[ReachableByResult] = {
    vec
      .groupBy { x =>
        (x.path.headOption ++ x.path.lastOption, x.partial, x.callDepth)
      }
      .map { case (_, list) =>
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

/** The execution context for the data flow engine.
  * @param semantics
  *   pre-determined semantic models for method calls e.g., logical operators, operators for common data structures.
  * @param config
  *   additional configurations for the data flow engine.
  */
case class EngineContext(semantics: Semantics, config: EngineConfig = EngineConfig())

/** Various configurations for the data flow engine.
  * @param maxCallDepth
  *   the k-limit for calls and field accesses.
  * @param initialTable
  *   an initial (starting node) -> (path-edges) cache to initiate data flow queries with.
  * @param shareCacheBetweenTasks
  *   enables sharing of previously calculated paths among other tasks.
  */
case class EngineConfig(
  var maxCallDepth: Int = 2,
  initialTable: Option[ResultTable] = None,
  shareCacheBetweenTasks: Boolean = true
)

/** Tracks various performance characteristics of the query engine.
  */
object QueryEngineStatistics extends Enumeration {

  type QueryEngineStatistic = Value

  val PATH_CACHE_HITS, PATH_CACHE_MISSES = Value

  private val statistics = new ConcurrentHashMap[QueryEngineStatistic, Long]()

  reset()

  /** Adds the given value to the associated value to the given [[QueryEngineStatistics]] key.
    * @param key
    *   the key associated with the value to transform.
    * @param value
    *   the value to add to the statistic. Can be negative.
    */
  def incrementBy(key: QueryEngineStatistic, value: Long): Unit =
    statistics.put(key, statistics.getOrDefault(key, 0L) + value)

  /** The results of the measured statistics.
    * @return
    *   a map of each [[QueryEngineStatistic]] and the associated value measurement.
    */
  def results(): Map[QueryEngineStatistic, Long] = statistics.asScala.toMap

  /** Sets all the tracked values back to 0.
    */
  def reset(): Unit =
    QueryEngineStatistics.values.map((_, 0L)).foreach { case (v, t) => statistics.put(v, t) }

}
