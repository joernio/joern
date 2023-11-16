package io.joern.x2cpg.passes.controlflow.cfgdominator

import io.shiftleft.semanticcpg.language.NodeOrdering

import scala.collection.mutable

class CfgDominator[NodeType](adapter: CfgAdapter[NodeType]) {

  /** Calculates the immediate dominators of all CFG nodes reachable from cfgEntry. Since the cfgEntry does not have an
    * immediate dominator, it has no entry in the return map.
    *
    * The algorithm is from: "A Simple, Fast Dominance Algorithm" from "Keith D. Cooper, Timothy J. Harvey, and Ken
    * Kennedy".
    */
  def calculate(cfgEntry: NodeType): mutable.LinkedHashMap[NodeType, NodeType] = {
    val UNDEFINED               = -1
    def expand(x: NodeType)     = { adapter.successors(x).iterator }
    def expandBack(x: NodeType) = { adapter.predecessors(x).iterator }

    val postOrderNumbering      = NodeOrdering.postOrderNumbering(cfgEntry, expand)
    val nodesInReversePostOrder = NodeOrdering.reverseNodeList(postOrderNumbering.toList).filterNot(_ == cfgEntry)
    // Index of each node into dominators array.
    val indexOf = postOrderNumbering.withDefaultValue(UNDEFINED)
    // We use withDefault because unreachable/dead
    // code nodes are not numbered but may be touched
    // as predecessors of reachable nodes.

    val dominators = Array.fill(indexOf.size)(UNDEFINED)
    dominators(indexOf(cfgEntry)) = indexOf(cfgEntry)

    /* Retrieve index of immediate dominator for node with given index. If the index is `UNDEFINED`, UNDEFINED is
     * returned. */
    def safeDominators(index: Int): Int = {
      if (index != UNDEFINED) {
        dominators(index)
      } else {
        UNDEFINED
      }
    }

    var changed = true
    while (changed) {
      changed = false
      nodesInReversePostOrder.foreach { node =>
        val firstNotUndefinedPred = expandBack(node).find { predecessor =>
          safeDominators(indexOf(predecessor)) != UNDEFINED
        }.get

        var newImmediateDominator = indexOf(firstNotUndefinedPred)
        expandBack(node).foreach { predecessor =>
          val predecessorIndex = indexOf(predecessor)
          if (safeDominators(predecessorIndex) != UNDEFINED) {
            newImmediateDominator = intersect(dominators, predecessorIndex, newImmediateDominator)
          }
        }

        val nodeIndex = indexOf(node)
        if (dominators(nodeIndex) != newImmediateDominator) {
          dominators(nodeIndex) = newImmediateDominator
          changed = true
        }
      }

    }

    val postOrderNumberingToNode = postOrderNumbering.map { case (node, index) => (index, node) }

    postOrderNumbering.collect {
      case (node, index) if node != cfgEntry =>
        val immediateDominatorIndex = dominators(index)
        (node, postOrderNumberingToNode(immediateDominatorIndex))
    }
  }

  private def intersect(dominators: Array[Int], immediateDomIndex1: Int, immediateDomIndex2: Int): Int = {
    var finger1 = immediateDomIndex1
    var finger2 = immediateDomIndex2

    while (finger1 != finger2) {
      while (finger1 < finger2) {
        finger1 = dominators(finger1)
      }
      while (finger2 < finger1) {
        finger2 = dominators(finger2)
      }
    }
    finger1
  }

}
