package io.joern.dataflowengineoss.passes.reachingdef

import io.shiftleft.codepropertygraph.generated.nodes.StoredNode

/** A general data flow problem, formulated as in the Dragon Book, Second Edition on page 626, with mild modifications.
  * In particular, instead of allowing only for the specification of a boundary, we allow initialization of IN and OUT.
  */
class DataFlowProblem[V](
  val flowGraph: FlowGraph,
  val transferFunction: TransferFunction[V],
  val meet: (V, V) => V,
  val inOutInit: InOutInit[V],
  val forward: Boolean,
  val empty: V
)

/** In essence, the flow graph is the control flow graph, however, we can compensate for small deviations from our
  * generic control flow graph to one that is better suited for solving data flow problems. In particular, method
  * parameters are not part of our normal control flow graph. By defining successors and predecessors, we provide a
  * wrapper that takes care of these minor discrepancies.
  */
trait FlowGraph {
  val entryNode: StoredNode
  val exitNode: StoredNode
  val allNodesReversePostOrder: List[StoredNode]
  val allNodesPostOrder: List[StoredNode]
  val succ: Map[StoredNode, List[StoredNode]]
  val pred: Map[StoredNode, List[StoredNode]]
}

/** This is actually a function family consisting of one transfer function for each node of the flow graph. Each
  * function maps from the analysis domain to the analysis domain, e.g., for reaching definitions, sets of definitions
  * are mapped to sets of definitions.
  */
trait TransferFunction[V] {
  def apply(n: StoredNode, x: V): V
}

/** As a practical optimization, OUT[N] is often initialized to GEN[N]. Moreover, we need a way of specifying boundary
  * conditions such as OUT[ENTRY] = {}. We achieve both by allowing the data flow problem to specify initializers for IN
  * and OUT.
  */
trait InOutInit[V] {

  def initIn: Map[StoredNode, V]

  def initOut: Map[StoredNode, V]

}

/** The solution consists of `in` and `out` for each node of the flow graph. We also attach the problem.
  */
case class Solution[T](in: Map[StoredNode, T], out: Map[StoredNode, T], problem: DataFlowProblem[T])
