package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode

class ResultExtractor(sinks: List[CfgNode]) {

  /** Traverse the table to generate results containing complete paths from this result table.
    */
  def extractResults(table: ResultTable): Vector[ReachableByResult] = {
    val sourceResults = table.keys().flatMap { key =>
      val r = table.table(key)
      r.filterNot(_.partial)
    }
    sourceResults.flatMap(x => recoverPaths(x, table))
  }

  private def recoverPaths(
    result: ReachableByResult,
    table: ResultTable,
    visited: Set[ReachableByResult] = Set()
  ): Vector[ReachableByResult] = {
    if (visited.contains(result)) {
      return Vector()
    }

    val resultsEndingHere = result.seed match {
      case Some(s) if sinks.contains(s) =>
        Vector(result.copy(path = result.path ++ Vector(PathElement(s))))
      case _ =>
        Vector()
    }

    val resultsViaChildren = result.seed
      .map { seed =>
        table.table.get(Key(seed, List())) match {
          case Some(entry) =>
            entry.flatMap { childResult =>
              recoverPaths(childResult, table, visited ++ Set(result)).map { c =>
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
