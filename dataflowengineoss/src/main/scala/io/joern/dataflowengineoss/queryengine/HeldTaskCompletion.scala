package io.joern.dataflowengineoss.queryengine

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

    deduplicateResultTable()
    val toProcess =
      heldTasks.distinct.sortBy(x =>
        (x.fingerprint.sink.id, x.fingerprint.callSiteStack.map(_.id).toString, x.callDepth)
      )
    var resultsProducedByTask: Map[ReachableByTask, Set[(TaskFingerprint, TableEntry)]] = Map()

    def allChanged  = toProcess.map { task => task.fingerprint -> true }.toMap
    def noneChanged = toProcess.map { t => t.fingerprint -> false }.toMap

    var changed: Map[TaskFingerprint, Boolean] = allChanged

    while (changed.values.toList.contains(true)) {
      val taskResultsPairs = toProcess
        .filter(t => changed(t.fingerprint))
        .par
        .map { t =>
          val resultsForTask = resultsForHeldTask(t).toSet
          val newResults     = resultsForTask -- resultsProducedByTask.getOrElse(t, Set())
          (t, resultsForTask, newResults)
        }
        .filter { case (_, _, newResults) => newResults.nonEmpty }
        .seq

      changed = noneChanged
      taskResultsPairs.foreach { case (t, resultsForTask, newResults) =>
        addCompletedTasksToMainTable(newResults.toList)
        newResults.foreach { case (fingerprint, _) =>
          changed += fingerprint -> true
        }
        resultsProducedByTask += (t -> resultsForTask)
      }
    }
    deduplicateResultTable()
  }

  private def resultsForHeldTask(heldTask: ReachableByTask): List[(TaskFingerprint, TableEntry)] = {
    resultTable.get(heldTask.fingerprint) match {
      case Some(results) =>
        results
          .flatMap { r =>
            createResultsForHeldTaskAndTableResult(heldTask, r)
          }
      case None => List()
    }
  }

  private def createResultsForHeldTaskAndTableResult(
    heldTask: ReachableByTask,
    result: TableEntry
  ): List[(TaskFingerprint, TableEntry)] = {
    val parentTasks = heldTask.taskStack.dropRight(1)
    val initialPath = heldTask.initialPath
    parentTasks
      .map { parentTask =>
        val stopIndex = initialPath
          .map(x => (x.node, x.callSiteStack))
          .indexOf((parentTask.sink, parentTask.callSiteStack)) + 1
        val initialPathOnlyUpToSink = initialPath.slice(0, stopIndex)
        val newPath                 = result.path ++ initialPathOnlyUpToSink
        (parentTask, TableEntry(newPath))
      }
      .filter { case (_, tableEntry) => containsCycle(tableEntry) }
  }

  private def containsCycle(tableEntry: TableEntry): Boolean = {
    val pathSeq = tableEntry.path.map(x => (x.node, x.callSiteStack, x.isOutputArg, x.outEdgeLabel))
    pathSeq.distinct.size == pathSeq.size
  }

  private def addCompletedTasksToMainTable(results: List[(TaskFingerprint, TableEntry)]): Unit = {
    results.groupBy(_._1).foreach { case (fingerprint, resultList) =>
      val entries = resultList.map(_._2)
      val old     = resultTable.getOrElse(fingerprint, Vector()).toList
      resultTable.put(fingerprint, deduplicateTableEntries(old ++ entries))
    }
  }

  private def deduplicateResultTable(): Unit = {
    resultTable.keys.foreach { key =>
      val results = resultTable(key)
      resultTable.put(key, deduplicateTableEntries(results))
    }
  }

  private def deduplicateTableEntries(list: List[TableEntry]): List[TableEntry] = {
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

}
