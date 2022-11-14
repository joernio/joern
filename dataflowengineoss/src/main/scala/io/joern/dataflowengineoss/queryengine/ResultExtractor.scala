package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode

import scala.collection.mutable

class ResultExtractor(table: ResultTable, sinks: List[CfgNode]) {

  private val knownPairs: mutable.Set[(CfgNode, CfgNode)] = mutable.Set()

  /** Traverse the table to generate results containing complete paths from this result table.
    */
  val results: Vector[ReachableByResult] = {
    val sourceResults = table.keys().flatMap { key =>
      val r = table.table(key)
      r.filterNot(_.partial)
    }
    sourceResults.flatMap(x => assemblePaths(x.path.head.node, x, table))
  }

  private def assemblePaths(
    source: CfgNode,
    result: ReachableByResult,
    table: ResultTable,
    visited: Set[ReachableByResult] = Set()
  ): Vector[ReachableByResult] = {
    if (visited.contains(result)) {
      return Vector()
    }

    val resultsEndingHere = result.seed match {
      case Some(s) if sinks.contains(s) =>
        if (knownPairs.contains((source, s))) {
          return Vector()
        }
        knownPairs.add((source, s))
        Vector(result.copy(path = result.path ++ Vector(PathElement(s))))
      case _ =>
        Vector()
    }

    val resultsViaChildren = result.seed
      .map { seed =>
        table.table.get(seed) match {
          case Some(entry) =>
            entry.flatMap { childResult =>
              assemblePaths(source, childResult, table, visited ++ Set(result)).map { c =>
                result.copy(path = result.path ++ c.path)
              }
            }
          case _ => Vector(result)
        }
      }
      .getOrElse(Vector(result))

    resultsEndingHere ++ resultsViaChildren
  }

}
