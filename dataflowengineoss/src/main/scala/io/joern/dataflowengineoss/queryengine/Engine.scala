package io.joern.dataflowengineoss.queryengine

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.passes.reachingdef.EdgeValidator
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.Edge
import overflowdb.traversal.{NodeOps, Traversal}

import java.util.concurrent._
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** @param taskStack
  *   The list of tasks that was solved to arrive at this task, including the current task, which is to be solved. The
  *   current task is the last element of the list.
  *
  * @param initialPath
  *   The path from the current sink downwards to previous sinks.
  *
  * @param callDepth
  *   This denotes how many call sites we have traversed before creating this path. We use this parameter to limit the
  *   depth of the analysis.
  */
case class ReachableByTask(taskStack: List[TaskFingerprint], initialPath: Vector[PathElement]) {

  /** This tasks fingerprint: if two tasks have the same fingerprint, then the TaskSolver MUST return the same result
    * for them. This is the basis of our caching scheme.
    */
  def fingerprint: TaskFingerprint = taskStack.last

  /** The sink at which we start the analysis (upwards)
    */
  def sink: CfgNode = fingerprint.sink

  /** The call sites we have expanded downwards during this analysis. We need to keep track of this so that we do not
    * end up expanding one call site and then returning to a different call site, which would produce an unreachable
    * path.
    */
  def callSiteStack: List[Call] = fingerprint.callSiteStack

  def callDepth: Int = fingerprint.callDepth

}

case class TaskSummary(tableEntries: Vector[(TaskFingerprint, TableEntry)], followupTasks: Vector[ReachableByTask])
case class TableEntry(path: Vector[PathElement])

/** The data flow engine allows determining paths to a set of sinks from a set of sources. To this end, it solves tasks
  * in parallel, creating and submitting new tasks upon completion of tasks. This class deals only with task scheduling,
  * while the creation of new tasks from existing tasks is handled by the class `TaskCreator`, and solving of tasks is
  * taken care of by the `TaskSolver`.
  */
class Engine(context: EngineContext) {

  import Engine._

  private val logger: Logger                   = LoggerFactory.getLogger(this.getClass)
  private val executorService: ExecutorService = Executors.newWorkStealingPool()
  private val completionService =
    new ExecutorCompletionService[TaskSummary](executorService)

  /** All results of tasks are accumulated in this table. At the end of the analysis, we extract results from the table
    * and return them.
    */
  private val mainResultTable: mutable.Map[TaskFingerprint, List[TableEntry]] = mutable.Map()
  private var numberOfTasksRunning: Int                                       = 0
  private val started: mutable.Buffer[ReachableByTask]                        = mutable.Buffer()
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
      addResultsToMainTable(newResults)
    }

    def addResultsToMainTable(results: Vector[(TaskFingerprint, TableEntry)]): Unit = {
      results.groupBy(_._1).foreach { case (fingerprint, resultList) =>
        val entries = resultList.map(_._2).toList
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
    runUntilAllTasksAreSolved()
    deduplicateResultTable()
    completeHeldTasks()
    deduplicateResultTable()
    deduplicateFinal(extractResultsFromTable(sinks))
  }

  private def deduplicateResultTable(): Unit = {
    mainResultTable.keys.foreach { key =>
      val results = mainResultTable(key)
      mainResultTable.put(key, deduplicateTableEntries(results))
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

  private def addCompletedTasksToMainTable(results: List[(TaskFingerprint, TableEntry)]): Unit = {
    results.groupBy(_._1).foreach { case (fingerprint, resultList) =>
      val entries = resultList.map(_._2)
      val old     = mainResultTable.getOrElse(fingerprint, Vector()).toList
      mainResultTable.put(fingerprint, deduplicateTableEntries(old ++ entries))
    }
  }

  private def submitTasks(tasks: Vector[ReachableByTask], sources: Set[CfgNode]) = {
    tasks.foreach { task =>
      if (started.exists(x => x.fingerprint == task.fingerprint)) {
        held ++= Vector(task)
      } else {
        started ++= Vector(task)
        numberOfTasksRunning += 1
        completionService.submit(new TaskSolver(task, context, sources))
      }
    }
  }

  /** Add results produced by held task until no more change can be observed.
    */
  private def completeHeldTasks(): Unit = {
    val toProcess =
      held.distinct.sortBy(x => (x.fingerprint.sink.id, x.fingerprint.callSiteStack.map(_.id).toString, x.callDepth))
    var resultsProducedByTask: Map[ReachableByTask, Set[(TaskFingerprint, TableEntry)]] = Map()
    var changed: Boolean                                                                = true
    while (changed) {
      changed = false
      val taskResultsPairs = toProcess.par.map { t =>
        val resultsForTask = resultsForHeldTask(t).toSet
        val newResults     = resultsForTask -- resultsProducedByTask.getOrElse(t, Set())
        (t, resultsForTask, newResults)
      }.seq

      taskResultsPairs.foreach { case (t, resultsForTask, newResults) =>
        if (newResults.nonEmpty) {
          addCompletedTasksToMainTable(newResults.toList)
          changed = true
          resultsProducedByTask += (t -> resultsForTask)
        }
      }
    }
  }

  private def resultsForHeldTask(heldTask: ReachableByTask): List[(TaskFingerprint, TableEntry)] = {
    mainResultTable.get(heldTask.fingerprint) match {
      case Some(results) =>
        results
          .flatMap { r =>
            createResultsForHeldTaskAndTableResult(heldTask, r)
          }
          .filter { case (_, tableEntry) =>
            // Do not allow paths with loops
            val pathSeq = tableEntry.path.map(x => (x.node, x.callSiteStack, x.isOutputArg, x.outEdgeLabel))
            pathSeq.distinct.size == pathSeq.size
          }
      case None => List()
    }
  }

  private def createResultsForHeldTaskAndTableResult(
    heldTask: ReachableByTask,
    result: TableEntry
  ): List[(TaskFingerprint, TableEntry)] = {
    heldTask.taskStack
      .dropRight(1)
      .indices
      .map { i =>
        val parentTask = heldTask.taskStack(i)
        val initialPathOnlyUpToSink =
          heldTask.initialPath.slice(
            0,
            heldTask.initialPath
              .map(x => (x.node, x.callSiteStack))
              .indexOf((parentTask.sink, parentTask.callSiteStack)) + 1
          )
        val newPath = result.path ++ initialPathOnlyUpToSink
        (parentTask, TableEntry(newPath))
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
    val outLabel = Some(e.property(Properties.VARIABLE)).getOrElse("")

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
              val internalMethodsForCall = parentNodeCall.flatMap(methodsForCall).to(Traversal).internal
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
        methodsForCall(call)
          .to(Traversal)
          .internal
          .isNotStub
          .nonEmpty && semanticsForCall(call).isEmpty
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

  def deduplicateTableEntries(list: List[TableEntry]): List[TableEntry] = {
    list
      .groupBy { result =>
        val head = result.path.headOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
        val last = result.path.lastOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
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

  def deduplicateFinal(list: List[TableEntry]): List[TableEntry] = {
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

  def deduplicateResults(vec: Vector[ReachableByResult]): Vector[ReachableByResult] = {
    vec
      .groupBy { result =>
        val head = result.path.headOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
        val last = result.path.lastOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
        (head, last, result.partial, result.callDepth)
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
            x.callDepth.toString + " " +
              x.taskStack
                .map(x => x.sink.id.toString + ":" + x.callSiteStack.map(_.id).mkString("|"))
                .toString + " " + x.path
                .map(x => (x.node.id, x.callSiteStack.map(_.id), x.visible, x.isOutputArg, x.outEdgeLabel).toString)
                .mkString("-")
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
case class EngineContext(semantics: Semantics = DefaultSemantics(), config: EngineConfig = EngineConfig())

/** Various configurations for the data flow engine.
  * @param maxCallDepth
  *   the k-limit for calls and field accesses.
  * @param initialTable
  *   an initial (starting node) -> (path-edges) cache to initiate data flow queries with.
  * @param shareCacheBetweenTasks
  *   enables sharing of previously calculated paths among other tasks.
  */
case class EngineConfig(
  var maxCallDepth: Int = 4,
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
