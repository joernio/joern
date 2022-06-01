package io.joern.x2cpg.passes.controlflow.cfgcreation

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** A control flow graph that is under construction, consisting of:
  *
  * @param entryNode
  *   the control flow graph's first node, that is, the node to which a CFG that appends this CFG should attach itself
  *   to.
  * @param fringe
  *   nodes of the CFG for which an outgoing edge type is already known but the destination node is not. These nodes are
  *   connected when another CFG is appended to this CFG.
  */
case class Cfg(entryNode: Option[CfgNode] = None, fringe: List[(CfgNode, CfgEdgeType)] = List()) {

  import Cfg._

  def isEmpty: Boolean = this == Cfg.empty

  private val logger = LoggerFactory.getLogger(getClass)

  /** Create a new CFG in which `other` is appended to this CFG. All nodes of the fringe are connected to `other`'s
    * entry node and the new fringe is `other`'s fringe. The diffgraphs, jumps, and labels are the sum of those present
    * in `this` and `other`.
    */
  def connect(acc: CfgAccumulator, other: Cfg): Cfg = {
    if (other.isEmpty) return this
    else if (this.isEmpty) return other

    acc.addEdges(edgesFromFringeTo(this, other.entryNode))
    Cfg(entryNode = this.entryNode, fringe = other.fringe)
  }
  def connect(acc: CfgAccumulator, others: Cfg*): Cfg = {
    var res = this
    for (other <- others) res = res.connect(acc, other)
    res
  }

  def withFringeEdgeType(cfgEdgeType: CfgEdgeType): Cfg = {
    this.copy(fringe = fringe.map { case (x, _) => (x, cfgEdgeType) })
  }
}

case class CfgEdge(src: CfgNode, dst: CfgNode, edgeType: CfgEdgeType)

object Cfg {

  /** The safe "null" Cfg.
    */
  val empty: Cfg = new Cfg()

  trait CfgEdgeType
  object TrueEdge extends CfgEdgeType {
    override def toString: String = "TrueEdge"
  }
  object FalseEdge extends CfgEdgeType {
    override def toString: String = "FalseEdge"
  }
  object AlwaysEdge extends CfgEdgeType {
    override def toString: String = "AlwaysEdge"
  }
  object CaseEdge extends CfgEdgeType {
    override def toString: String = "CaseEdge"
  }

  /** Create edges from all nodes of cfg's fringe to `node`.
    */
  def edgesFromFringeTo(cfg: Cfg, node: Option[CfgNode]): List[CfgEdge] = {
    node
      .map { dstNode =>
        cfg.fringe.map { case (sourceNode, cfgEdgeType) =>
          CfgEdge(sourceNode, dstNode, cfgEdgeType)
        }
      }
      .getOrElse(Nil)
  }

  /** Create edges of given type from a list of source nodes to a destination node
    */
  def edges(sources: List[CfgNode], dstNode: Option[CfgNode], cfgEdgeType: CfgEdgeType = AlwaysEdge): List[CfgEdge] = {
    edgesToMultiple(sources, dstNode.toList, cfgEdgeType)
  }

  def singleEdge(source: CfgNode, destination: CfgNode, cfgEdgeType: CfgEdgeType = AlwaysEdge): List[CfgEdge] = {
    edgesToMultiple(List(source), List(destination), cfgEdgeType)
  }

  /** Create edges of given type from all nodes in `sources` to `node`.
    */
  def edgesToMultiple(
    sources: List[CfgNode],
    destinations: List[CfgNode],
    cfgEdgeType: CfgEdgeType = AlwaysEdge
  ): List[CfgEdge] = {

    sources.flatMap { l =>
      destinations.map { n =>
        CfgEdge(l, n, cfgEdgeType)
      }
    }
  }

}

class CfgAccumulator(
  val edgeBuffer: mutable.ArrayBuffer[CfgEdge] = mutable.ArrayBuffer.empty,
  val labeledNodes: mutable.HashMap[String, CfgNode] = mutable.HashMap.empty,
  var breaks: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty,
  var continues: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty,
  var caseLabels: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty,
  val jumpsToLabel: mutable.ArrayBuffer[(CfgNode, String)] = mutable.ArrayBuffer.empty
) {
  def addEdges(edges: List[CfgEdge]): this.type = {
    edgeBuffer.appendAll(edges)
    this
  }

  def swapBreaks(other: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty): ArrayBuffer[CfgNode] = {
    val tmp = breaks
    breaks = other
    tmp
  }
  def swapContinues(other: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty): ArrayBuffer[CfgNode] = {
    val tmp = continues
    continues = other
    tmp
  }
  def swapCaseLabels(other: mutable.ArrayBuffer[CfgNode] = mutable.ArrayBuffer.empty): ArrayBuffer[CfgNode] = {
    val tmp = caseLabels
    caseLabels = other
    tmp
  }
}
