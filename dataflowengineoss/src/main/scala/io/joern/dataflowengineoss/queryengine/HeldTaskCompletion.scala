package io.joern.dataflowengineoss.queryengine
import io.joern.dataflowengineoss.queryengine.Engine.deduplicateTableEntries

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

/** Complete held tasks using the result table. The result table is modified in the process.
  */
class HeldTaskCompletion(
  heldTasks: List[ReachableByTask],
  resultTable: mutable.Map[TaskFingerprint, List[TableEntry]]
) {

  /** Add results produced by held task until no more change can be observed.
    */
  def completeHeldTasks(): Unit = {
    val toProcess =
      heldTasks.distinct.sortBy(x =>
        (x.fingerprint.sink.id, x.fingerprint.callSiteStack.map(_.id).toString, x.callDepth)
      )
    var resultsProducedByTask: Map[ReachableByTask, Set[(TaskFingerprint, TableEntry)]] = Map()
    var changed: Map[TaskFingerprint, Boolean] = toProcess.map { task => task.fingerprint -> true }.toMap

    while (changed.values.toList.contains(true)) {
      val taskResultsPairs = toProcess
        .filter(t => changed(t.fingerprint))
        .par
        .map { t =>
          val resultsForTask = resultsForHeldTask(t).toSet
          val newResults     = resultsForTask -- resultsProducedByTask.getOrElse(t, Set())
          (t, resultsForTask, newResults)
        }
        .seq

      changed = toProcess.map { t => t.fingerprint -> false }.toMap

      taskResultsPairs.foreach { case (t, resultsForTask, newResults) =>
        if (newResults.nonEmpty) {
          addCompletedTasksToMainTable(newResults.toList)
          newResults.foreach { case (fingerprint, _) =>
            changed += fingerprint -> true
          }
          resultsProducedByTask += (t -> resultsForTask)
        }
      }
    }
  }

  private def resultsForHeldTask(heldTask: ReachableByTask): List[(TaskFingerprint, TableEntry)] = {
    resultTable.get(heldTask.fingerprint) match {
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
      .map { parentTask =>
        val stopIndex = heldTask.initialPath
          .map(x => (x.node, x.callSiteStack))
          .indexOf((parentTask.sink, parentTask.callSiteStack)) + 1
        val initialPathOnlyUpToSink =
          heldTask.initialPath.slice(0, stopIndex)
        val newPath = result.path ++ initialPathOnlyUpToSink
        (parentTask, TableEntry(newPath))
      }
  }

  private def addCompletedTasksToMainTable(results: List[(TaskFingerprint, TableEntry)]): Unit = {
    results.groupBy(_._1).foreach { case (fingerprint, resultList) =>
      val entries = resultList.map(_._2)
      val old     = resultTable.getOrElse(fingerprint, Vector()).toList
      resultTable.put(fingerprint, deduplicateTableEntries(old ++ entries))
    }
  }

}
