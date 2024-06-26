package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.passes.reachingdef.EdgeValidator
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.Edge

import java.util.concurrent.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** The data flow engine allows determining paths to a set of sinks from a set of sources. To this end, it solves tasks
  * in parallel, creating and submitting new tasks upon completion of tasks. This class deals only with task scheduling,
  * while the creation of new tasks from existing tasks is handled by the class `TaskCreator`, and solving of tasks is
  * taken care of by the `TaskSolver`.
  */
class Engine(context: EngineContext) {

  import Engine.*

  private val logger: Logger                   = LoggerFactory.getLogger(this.getClass)
  private val executorService: ExecutorService = Executors.newWorkStealingPool()
  private val completionService =
    new ExecutorCompletionService[TaskSummary](executorService)

  /** All results of tasks are accumulated in this table. At the end of the analysis, we extract results from the table
    * and return them.
    */
  private val mainResultTable: mutable.Map[TaskFingerprint, List[TableEntry]] = mutable.Map()
  private var numberOfTasksRunning: Int                                       = 0
  private val started: mutable.HashSet[TaskFingerprint]                       = mutable.HashSet[TaskFingerprint]()
  private val held: mutable.Buffer[ReachableByTask]                           = mutable.Buffer()

  /** Determine flows from sources to sinks by exploring the graph backwards from sinks to sources. Returns the list of
    * results along with a ResultTable, a cache of known paths created during the analysis.
    */
  def backwards(sinks: List[CfgNode], sources: List[CfgNode]): List[TableEntry] = {
    if (sources.isEmpty) {
      logger.info("Attempting to determine flows from empty list of sources.")
    }
    if (sinks.isEmpty) {
      logger.info("Attempting to determine flows to empty list of sinks.")
    }
    reset()
    val sourcesSet = sources.toSet
    val tasks      = createOneTaskPerSink(sinks)
    solveTasks(tasks, sourcesSet, sinks)
  }

  private def reset(): Unit = {
    mainResultTable.clear()
    numberOfTasksRunning = 0
    started.clear()
    held.clear()
  }

  private def createOneTaskPerSink(sinks: List[CfgNode]) = {
    sinks.map(sink => ReachableByTask(List(TaskFingerprint(sink, List(), 0)), Vector()))
  }

  /** Submit tasks to a worker pool, solving them in parallel. Upon receiving results for a task, new tasks are
    * submitted accordingly. Once no more tasks can be created, the list of results is returned.
    */
  private def solveTasks(
    tasks: List[ReachableByTask],
    sources: Set[CfgNode],
    sinks: List[CfgNode]
  ): List[TableEntry] = {

    /** Solving a task produces a list of summaries. The following method is called for each of these summaries. It
      * submits new tasks and adds results to the result table.
      */
    def handleSummary(taskSummary: TaskSummary): Unit = {
      val newTasks = taskSummary.followupTasks
      submitTasks(newTasks, sources)
      val newResults = taskSummary.tableEntries
      addEntriesToMainTable(newResults)
    }

    def addEntriesToMainTable(entries: Vector[(TaskFingerprint, TableEntry)]): Unit = {
      entries.groupBy(_._1).foreach { case (fingerprint, entryList) =>
        val entries = entryList.map(_._2).toList
        mainResultTable.updateWith(fingerprint) {
          case Some(list) => Some(list ++ entries)
          case None       => Some(entries)
        }
      }
    }

    def runUntilAllTasksAreSolved(): Unit = {
      while (numberOfTasksRunning > 0) {
        Try {
          completionService.take.get
        } match {
          case Success(resultsOfTask) =>
            numberOfTasksRunning -= 1
            handleSummary(resultsOfTask)
          case Failure(exception) =>
            numberOfTasksRunning -= 1
            logger.warn(s"SolveTask failed with exception:", exception)
            exception.printStackTrace()
        }
      }
    }

    submitTasks(tasks.toVector, sources)
    val startTimeSec: Long = System.currentTimeMillis / 1000
    runUntilAllTasksAreSolved()
    val taskFinishTimeSec: Long = System.currentTimeMillis / 1000
    logger.debug(
      "Time measurement -----> Task processing done in " +
        (taskFinishTimeSec - startTimeSec) + " seconds"
    )
    new HeldTaskCompletion(held.toList, mainResultTable).completeHeldTasks()
    val dedupResult          = deduplicateFinal(extractResultsFromTable(sinks))
    val allDoneTimeSec: Long = System.currentTimeMillis / 1000

    logger.debug(
      "Time measurement -----> Task processing: " +
        (taskFinishTimeSec - startTimeSec) + " seconds" +
        ", Deduplication: " + (allDoneTimeSec - taskFinishTimeSec) +
        ", Deduped results size: " + dedupResult.length
    )
    dedupResult
  }

  private def submitTasks(tasks: Vector[ReachableByTask], sources: Set[CfgNode]): Unit = {
    tasks.foreach { task =>
      if (started.contains(task.fingerprint)) {
        held ++= Vector(task)
      } else {
        started.add(task.fingerprint)
        numberOfTasksRunning += 1
        completionService.submit(new TaskSolver(task, context, sources))
      }
    }
  }

  private def extractResultsFromTable(sinks: List[CfgNode]): List[TableEntry] = {
    sinks.flatMap { sink =>
      mainResultTable.get(TaskFingerprint(sink, List(), 0)) match {
        case Some(results) => results
        case _             => Vector()
      }
    }
  }

  private def deduplicateFinal(list: List[TableEntry]): List[TableEntry] = {
    list
      .groupBy { result =>
        val head = result.path.head.node
        val last = result.path.last.node
        (head, last)
      }
      .map { case (_, list) =>
        val lenIdPathPairs = list.map(x => (x.path.length, x))
        val withMaxLength = (lenIdPathPairs.sortBy(_._1).reverse match {
          case Nil    => Nil
          case h :: t => h :: t.takeWhile(y => y._1 == h._1)
        }).map(_._2)

        if (withMaxLength.length == 1) {
          withMaxLength.head
        } else {
          withMaxLength.minBy { x =>
            x.path
              .map(x => (x.node.id, x.callSiteStack.map(_.id), x.visible, x.isOutputArg, x.outEdgeLabel).toString)
              .mkString("-")
          }
        }
      }
      .toList
  }

  /** This must be called when one is done using the engine.
    */
  def shutdown(): Unit = {
    executorService.shutdown()
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
  def expandIn(curNode: CfgNode, path: Vector[PathElement], callSiteStack: List[Call] = List())(implicit
    semantics: Semantics
  ): Vector[PathElement] = {
    ddgInE(curNode, path, callSiteStack).flatMap(x => elemForEdge(x, callSiteStack))
  }

  private def elemForEdge(e: Edge, callSiteStack: List[Call] = List())(implicit
    semantics: Semantics
  ): Option[PathElement] = {
    val curNode  = e.inNode().asInstanceOf[CfgNode]
    val parNode  = e.outNode().asInstanceOf[CfgNode]
    val outLabel = Some(e.property(Properties.Variable)).getOrElse("")

    if (!EdgeValidator.isValidEdge(curNode, parNode)) {
      return None
    }

    curNode match {
      case childNode: Expression =>
        parNode match {
          case parentNode: Expression =>
            val parentNodeCall = parentNode.inCall.l
            val sameCallSite   = parentNode.inCall.l == childNode.start.inCall.l
            val visible = if (sameCallSite) {
              val semanticExists         = parentNode.semanticsForCallByArg.nonEmpty
              val internalMethodsForCall = parentNodeCall.flatMap(methodsForCall).internal
              (semanticExists && parentNode.isDefined) || internalMethodsForCall.isEmpty
            } else {
              parentNode.isDefined
            }
            val isOutputArg = isOutputArgOfInternalMethod(parentNode)
            Some(PathElement(parentNode, callSiteStack, visible, isOutputArg, outEdgeLabel = outLabel))
          case parentNode if parentNode != null =>
            Some(PathElement(parentNode, callSiteStack, outEdgeLabel = outLabel))
          case null =>
            None
        }
      case _ =>
        Some(PathElement(parNode, callSiteStack, outEdgeLabel = outLabel))
    }
  }

  def isOutputArgOfInternalMethod(arg: Expression)(implicit semantics: Semantics): Boolean = {
    arg.inCall.l match {
      case List(call) =>
        methodsForCall(call).internal.isNotStub.nonEmpty && semanticsForCall(call).isEmpty
      case _ =>
        false
    }
  }

  /** For a given node `node`, return all incoming reaching definition edges, unless the source node is (a) a METHOD
    * node, (b) already present on `path`, or (c) a CALL node to a method where the semantic indicates that taint is
    * propagated to it.
    */
  private def ddgInE(node: CfgNode, path: Vector[PathElement], callSiteStack: List[Call] = List()): Vector[Edge] = {
    node
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .filter { e =>
        e.outNode() match {
          case srcNode: CfgNode =>
            !srcNode.isInstanceOf[Method] && !path
              .map(x => x.node)
              .contains(srcNode)
          case _ => false
        }
      }
      .toVector
  }

  def argToOutputParams(arg: Expression): Iterator[MethodParameterOut] = {
    argToMethods(arg).parameter
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
    methodsForCall(call).internal.nonEmpty
  }
  def isCallToInternalMethodWithoutSemantic(call: Call)(implicit semantics: Semantics): Boolean = {
    isCallToInternalMethod(call) && semanticsForCall(call).isEmpty
  }

  def semanticsForCall(call: Call)(implicit semantics: Semantics): List[FlowSemantic] = {
    Engine.methodsForCall(call).flatMap { method =>
      semantics.forMethod(method.fullName)
    }
  }

}

/** The execution context for the data flow engine.
  * @param semantics
  *   pre-determined semantic models for method calls e.g., logical operators, operators for common data structures.
  * @param config
  *   additional configurations for the data flow engine.
  */
case class EngineContext(semantics: Semantics = DefaultSemantics(), config: EngineConfig = EngineConfig())

/** Various configurations for the data flow engine.
  * @param maxCallDepth
  *   the k-limit for calls and field accesses.
  * @param initialTable
  *   an initial (starting node) -> (path-edges) cache to initiate data flow queries with.
  * @param shareCacheBetweenTasks
  *   enables sharing of previously calculated paths among other tasks.
  * @param maxArgsToAllow
  *   max limit to determine all corresponding arguments at all call sites to the method
  * @param maxOutputArgsExpansion
  *   max limit on number arguments for which tasks will be created for unresolved arguments
  */
case class EngineConfig(
  var maxCallDepth: Int = 4,
  initialTable: Option[mutable.Map[TaskFingerprint, Vector[ReachableByResult]]] = None,
  shareCacheBetweenTasks: Boolean = true,
  maxArgsToAllow: Int = 1000,
  maxOutputArgsExpansion: Int = 1000
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
