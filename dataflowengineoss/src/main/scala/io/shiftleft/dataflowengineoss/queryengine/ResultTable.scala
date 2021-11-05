package io.shiftleft.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}

import scala.jdk.CollectionConverters._

class ResultTable {

  private val table = new java.util.concurrent.ConcurrentHashMap[StoredNode, Vector[ReachableByResult]].asScala

  /**
    * Add all results in `value` to table entry at `key`, appending to existing
    * results.
    */
  def add(key: StoredNode, value: Vector[ReachableByResult]): Unit = {
    table.asJava.compute(key, { (_, existingValue) =>
      Option(existingValue).toVector.flatten ++ value
    })
  }

  /**
    * For a given path, determine whether results for the first element (`first`) are stored in the
    * table, and if so, for each result, determine the path up to `first` and prepend it to
    * `path`, giving us new results via table lookup.
    */
  def createFromTable(first: PathElement, remainder: Vector[PathElement]): Option[Vector[ReachableByResult]] = {
    table.get(first.node).map { res =>
      res.map { r =>
        val pathToFirstNode = r.path.slice(0, r.path.map(_.node).indexOf(first.node))
        val completePath = pathToFirstNode ++ (first +: remainder)
        r.copy(path = completePath)
      }
    }
  }

  /**
    * Retrieve list of results for `node` or None if they are not
    * available in the table.
    */
  def get(node: StoredNode): Option[Vector[ReachableByResult]] = {
    table.get(node)
  }

}

/**
  * A (partial) result, informing about a path that exists from a source to another
  * node in the graph.
  *
  * @param path this is the main result - a known path
  * @param partial indicate whether this result stands on its own or requires further analysis,
  *                e.g., by expanding output arguments backwards into method output parameters.
  * */
case class ReachableByResult(path: Vector[PathElement], callDepth: Int = 0, partial: Boolean = false) {
  def source: CfgNode = path.head.node

  def unresolvedArgs: Vector[CfgNode] =
    path.collect {
      case elem if !elem.resolved =>
        elem.node
    }.distinct
}

/**
  * We represent data flows as sequences of path elements, where each
  * path element consists of a node, flags and the label of its
  * outgoing edge.
  *
  * @param node The parent node
  * @param visible whether this path element should be shown in the flow
  * @param resolved whether we have resolved the method call this argument belongs to
  * @param outEdgeLabel label of the outgoing DDG edge
  * */
case class PathElement(node: CfgNode, visible: Boolean = true, resolved: Boolean = true, outEdgeLabel: String = "")
