package io.joern.x2cpg.passes.controlflow.cfgdominator

import scala.collection.mutable

/** Calculates the dominator frontier for a set of CFG nodes. The returned multimap associates the frontier nodes to
  * each CFG node.
  *
  * The used algorithm is from: "A Simple, Fast Dominance Algorithm" from "Keith D. Cooper, Timothy J. Harvey, and Ken
  * Kennedy".
  */
class CfgDominatorFrontier[NodeType](cfgAdapter: CfgAdapter[NodeType], domTreeAdapter: DomTreeAdapter[NodeType]) {

  private def doms(x: NodeType): Option[NodeType] = domTreeAdapter.immediateDominator(x)
  private def pred(x: NodeType): Seq[NodeType]    = cfgAdapter.predecessors(x).iterator.to(Seq)
  private def onlyJoinNodes(x: NodeType): Option[(NodeType, Seq[NodeType])] =
    Option(pred(x)).filter(_.size > 1).map(p => (x, p))
  private def withIDom(x: NodeType, preds: Seq[NodeType]) =
    doms(x).map(i => (x, preds, i))

  def calculate(cfgNodes: Iterator[NodeType]): mutable.Map[NodeType, mutable.Set[NodeType]] = {
    val domFrontier = mutable.Map.empty[NodeType, mutable.Set[NodeType]]

    for {
      cfgNode                         <- cfgNodes
      (nodeType, joinNodes)           <- onlyJoinNodes(cfgNode)
      (joinNode, preds, joinNodeIDom) <- withIDom(nodeType, joinNodes)
    } preds.foreach { p =>
      var currentPred = Option(p)
      while (currentPred.isDefined && currentPred.get != joinNodeIDom) {
        val frontierNodes = domFrontier.getOrElseUpdate(currentPred.get, mutable.Set.empty)
        frontierNodes.add(joinNode)
        currentPred = doms(currentPred.get)
      }
    }

    domFrontier
  }
}
