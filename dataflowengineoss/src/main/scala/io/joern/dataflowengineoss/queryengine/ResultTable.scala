package io.joern.dataflowengineoss.queryengine

import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, StoredNode}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** The Result Table is a cache that allows retrieving known paths for nodes, that is, paths that end in the node.
  */
class ResultTable(
  val table: mutable.Map[StoredNode, Vector[ReachableByResult]] =
    new java.util.concurrent.ConcurrentHashMap[StoredNode, Vector[ReachableByResult]].asScala
) {

  /** Add all results in `results` to table at `key`, appending to existing results.
    */
  def add(key: StoredNode, results: Vector[ReachableByResult]): Unit = {
    table.asJava.compute(
      key,
      { (_, existingValue) =>
        Option(existingValue).toVector.flatten ++ results
      }
    )
  }

  /** For a given path, determine whether results for the first element (`first`) are stored in the table, and if so,
    * for each result, determine the path up to `first` and prepend it to `path`, giving us new results via table
    * lookup.
    */
  def createFromTable(first: PathElement, remainder: Vector[PathElement]): Option[Vector[ReachableByResult]] = {
    table.get(first.node).map { res =>
      res.map { r =>
        val pathToFirstNode = r.path.slice(0, r.path.map(_.node).indexOf(first.node))
        val completePath    = pathToFirstNode ++ (first +: remainder)
        r.copy(path = Vector(completePath.head) ++ completePath.tail)
      }
    }
  }

  /** Retrieve list of results for `node` or None if they are not available in the table.
    */
  def get(node: StoredNode): Option[Vector[ReachableByResult]] = {
    table.get(node)
  }

  /** Returns all keys to allow for iteration through the table.
    */
  def keys(): Vector[StoredNode] = table.keys.toVector

}

/** A (partial) result, informing about a path that exists from a source to another node in the graph.
  *
  * @param path
  *   this is the main result - a known path
  * @param table
  *   the result table - kept to allow for detailed inspection of intermediate paths
  * @param resultType
  *   see ResultType below
  * @param callSiteStack
  *   the call site stack containing the call sites that were expanded to kick off the task. We require this to match
  *   call sites to exclude non-realizable paths through other callers
  */
case class ReachableByResult(
  path: Vector[PathElement],
  table: ResultTable,
  callSiteStack: mutable.Stack[Call],
  resultType: ResultType = ReachSource,
  callDepth: Int = 0
) {
  def startingPoint: CfgNode = path.head.node
}

sealed trait ResultType
sealed trait CompleteResult extends ResultType
sealed trait PartialResult  extends ResultType
// The Engine will collect partial results and create new tasks from them, see TaskCreator

case object ReachSource      extends CompleteResult
case object ReachParameterIn extends PartialResult
case object ReachArgument    extends PartialResult
case object ReachCall        extends PartialResult

/** We represent data flows as sequences of path elements, where each path element consists of a node, a visible flag
  * and the label of its outgoing edge.
  *
  * @param node
  *   The parent node
  * @param visible
  *   whether this path element should be shown in the flow
  * @param outEdgeLabel
  *   label of the outgoing DDG edge
  */
case class PathElement(node: CfgNode, visible: Boolean = true, outEdgeLabel: String = "")
