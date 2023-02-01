package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode}

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.language.postfixOps

/** Complete held tasks using the result table. The result table is modified in the process.
  *
  * Results obtained when completing a held task depend on the following:
  *
  * (a) the `initialPath` of the held task (path from the node where the task was held down to a sink)
  *
  * (b) the entries in the table for `heldTask.fingerprint`.
  *
  * Upon completing a task, new results are stored in the table for each task of its `taskStack`. This means that we may
  * not end up with all results when first completing a task because another task needs to be completed first so that
  * all results for `heldTask.fingerprint` are available. We address this problem by computing results in a loop until
  * no more changes can be observed.
  */
class HeldTaskCompletion(
  heldTasks: List[ReachableByTask],
  resultTable: mutable.Map[TaskFingerprint, List[TableEntry]]
) {

  /** Add results produced by held task until no more change can be observed.
    *
    * We use the following simple algorithm (that can possibly be optimized in the future):
    *
    * For each held `task`, we keep a Boolean `changed(task)`, which indicates whether new results for the `task` were
    * produced. We initialize the Booleans to be true. Computation is terminated when all Booleans are false, that is,
    * when no more changes in the result table can be observed.
    *
    * If we do detect a change, we determine all tasks for which changed results exist and recompute their results. We
    * compare the results with those produced previously (stored in `resultsProducedByTask`). If any new results were
    * created, `changed` is set to true for the result's table entry and `resultsProductByTask` is updated.
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
    val groupMap
      : mutable.Map[TaskFingerprint, Map[((CfgNode, List[Call], Boolean), (CfgNode, List[Call], Boolean)), List[
        TableEntry
      ]]] = mutable.Map()

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
        addCompletedTasksToMainTable(newResults.toList, groupMap)
        newResults.foreach { case (fingerprint, _) =>
          changed += fingerprint -> true
        }
        resultsProducedByTask += (t -> resultsForTask)
      }
    }
    deduplicateResultTable()
  }

  /** In essence, completing a held task simply means appending the path stored in the held task to all results that are
    * available for the held task in the table. In practice, we create one result for each task of the parent task's
    * `taskStack`, so that we do not only get a new result for the sink, but one for each of the parent nodes on the
    * way.
    */
  private def resultsForHeldTask(heldTask: ReachableByTask): List[(TaskFingerprint, TableEntry)] = {
    // Create a flat list of results by computing results for each
    // table entry and appending them.
    resultTable.get(heldTask.fingerprint) match {
      case Some(results) =>
        results
          .flatMap { r =>
            createResultsForHeldTaskAndTableResult(heldTask, r)
          }
      case None => List()
    }
  }

  /** This method creates a list of results from a held task and a table entry by appending paths of the held task to
    * the path stored in the held task (`initialPath`) up to each of its parent tasks.
    *
    * A possible optimization here is to store computed slices in a lazily populated table and attempt to look them up.
    */
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

  private def addCompletedTasksToMainTable(
    results: List[(TaskFingerprint, TableEntry)],
    groupMap: mutable.Map[TaskFingerprint, Map[((CfgNode, List[Call], Boolean), (CfgNode, List[Call], Boolean)), List[
      TableEntry
    ]]]
  ): Unit = {
    results.groupBy(_._1).par.foreach { case (fingerprint, resultList) =>
      val entries = resultList.map(_._2)
      val newGroups = entries
        .groupBy { result =>
          val head = result.path.headOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
          val last = result.path.lastOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
          (head, last)
        }

      val old = resultTable.getOrElse(fingerprint, Vector()).toList
      val oldGroups = groupMap.getOrElse(
        fingerprint,
        old
          .groupBy { result =>
            val head = result.path.headOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
            val last = result.path.lastOption.map(x => (x.node, x.callSiteStack, x.isOutputArg)).get
            (head, last)
          }
      )

      val mergedGroups = oldGroups ++ newGroups.par.map { case (k, v) =>
        k -> {
          val old = oldGroups.getOrElse(k, List())
          val maxLen = if (old.length > 0) {
            old.head.path.length
          } else { 0 }

          val gtOrEqualMax = v.filter(x => x.path.length >= maxLen)
          val gtMax        = gtOrEqualMax.filter(x => x.path.length > maxLen)

          if (gtMax.length > 0) {
            // new list contains elements with paths exceeding the max. retain new list elements only
            // that have max length
            var newMaxLen = maxLen
            gtMax.foreach(x => {
              if (x.path.length > newMaxLen) {
                newMaxLen = x.path.length
              }
            })
            val element = gtMax.filter(x => x.path.length == newMaxLen).par.minBy(computePriority)
            List(element)
          } else if (gtOrEqualMax == 0) {
            // new list contains all elements with paths less than the max. retain old list elements only
            old
          } else {
            // new list contains all elements with paths less than or equal to the max but not exceeding it.
            // append new list elements that are equal to max
            val element = (old ++ gtOrEqualMax.par.filter(x => x.path.length == maxLen)).par.minBy(computePriority)
            List(element)
          }
        }
      }
      val mergedList = mergedGroups.map { case (_, list) =>
        list.head
      }.toList
      synchronized(resultTable.put(fingerprint, mergedList))
      synchronized(groupMap.update(fingerprint, mergedGroups))
    }
  }

  private def deduplicateResultTable(): Unit = {
    resultTable.keys.foreach { key =>
      val results = resultTable(key)
      resultTable.put(key, deduplicateTableEntries(results))
    }
  }

  /** This method deduplicates the list of entries stored in a table cell.
    *
    * We treat entries as the same if their start and end point are the same. Points are given by nodes in the graph,
    * the `callSiteStack` and the `isOutputArg` flag.
    *
    * For a group of flows that we treat as the same, we select the flow with the maximum length. If there are multiple
    * flows with maximum length, then we compute a string representation of the flows - taking into account all fields
    *   - and select the flow with maximum length that is smallest in terms of this string representation.
    */
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

  private def getListFromGroups(
    groups: Map[((CfgNode, List[Call], Boolean), (CfgNode, List[Call], Boolean)), List[TableEntry]]
  ): List[TableEntry] = {
    val mapped = groups.map { case (_, list) =>
      list.head
    }
    mapped.toList
  }

  private def computePriority(entry: TableEntry): BigInt = {
    var priority: BigInt   = entry.path.length
    val multiplier: BigInt = 131072 // 2^17

    entry.path.foreach(element => {
      priority = priority + element.callSiteStack.length * multiplier
      priority = priority + element.node.id() * multiplier * 64
      priority = priority + element.isOutputArg.hashCode() * multiplier * 64
      priority = priority + element.visible.hashCode() * multiplier * 64
    })
    priority
  }

}
