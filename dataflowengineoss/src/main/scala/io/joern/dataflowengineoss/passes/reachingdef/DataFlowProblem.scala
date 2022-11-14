package io.joern.dataflowengineoss.passes.reachingdef

/** A general data flow problem, formulated as in the Dragon Book, Second Edition on page 626, with mild modifications.
  * In particular, instead of allowing only for the specification of a boundary, we allow initialization of IN and OUT.
  */
class DataFlowProblem[Node, V](
  val flowGraph: FlowGraph[Node],
  val transferFunction: TransferFunction[Node, V],
  val meet: (V, V) => V,
  val inOutInit: InOutInit[Node, V],
  val forward: Boolean,
  val empty: V
)

/** In essence, the flow graph is the control flow graph, however, we can compensate for small deviations from our
  * generic control flow graph to one that is better suited for solving data flow problems. In particular, method
  * parameters are not part of our normal control flow graph. By defining successors and predecessors, we provide a
  * wrapper that takes care of these minor discrepancies.
  */
trait FlowGraph[Node] {
  val allNodesReversePostOrder: List[Node]
  val allNodesPostOrder: List[Node]
  def succ(node: Node): IterableOnce[Node]
  def pred(node: Node): IterableOnce[Node]
}

/** This is actually a function family consisting of one transfer function for each node of the flow graph. Each
  * function maps from the analysis domain to the analysis domain, e.g., for reaching definitions, sets of definitions
  * are mapped to sets of definitions.
  */
trait TransferFunction[Node, V] {
  def apply(n: Node, x: V): V
}

/** As a practical optimization, OUT[N] is often initialized to GEN[N]. Moreover, we need a way of specifying boundary
  * conditions such as OUT[ENTRY] = {}. We achieve both by allowing the data flow problem to specify initializers for IN
  * and OUT.
  */
trait InOutInit[Node, V] {

  def initIn: Map[Node, V]

  def initOut: Map[Node, V]

}

/** The solution consists of `in` and `out` for each node of the flow graph. We also attach the problem.
  */
case class Solution[Node, V](in: Map[Node, V], out: Map[Node, V], problem: DataFlowProblem[Node, V])
