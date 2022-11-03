package io.joern.x2cpg.passes.controlflow.cfgcreation

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.CfgEdgeType
import org.slf4j.LoggerFactory

/** A control flow graph that is under construction, consisting of:
  *
  * @param entryNode
  *   the control flow graph's first node, that is, the node to which a CFG that appends this CFG should attach itself
  *   to.
  * @param edges
  *   control flow edges between nodes of the code property graph.
  * @param fringe
  *   nodes of the CFG for which an outgoing edge type is already known but the destination node is not. These nodes are
  *   connected when another CFG is appended to this CFG.
  *
  * In addition to these three core building blocks, we store labels and jump statements that have not been resolved and
  * may be resolvable as parent sub trees or sibblings are translated.
  *
  * @param labeledNodes
  *   labels contained in the abstract syntax tree from which this CPG was generated
  * @param caseLabels
  *   labels beginning with "case"
  * @param breaks
  *   unresolved breaks collected along the way together with an integer value which indicates the number of loop/switch
  *   levels to break
  * @param continues
  *   unresolved continues collected along the way together with an integer value which indicates the number of
  *   loop/switch levels after which to continue
  * @param jumpsToLabel
  *   unresolved gotos, labeled break and labeld continues collected along the way
  */
case class Cfg(
  entryNode: Option[CfgNode] = None,
  edges: List[CfgEdge] = List(),
  fringe: List[(CfgNode, CfgEdgeType)] = List(),
  labeledNodes: Map[String, CfgNode] = Map(),
  breaks: List[(CfgNode, Int)] = List(),
  continues: List[(CfgNode, Int)] = List(),
  caseLabels: List[CfgNode] = List(),
  jumpsToLabel: List[(CfgNode, String)] = List()
) {

  import Cfg._

  /** Create a new CFG in which `other` is appended to this CFG. All nodes of the fringe are connected to `other`'s
    * entry node and the new fringe is `other`'s fringe. The diffgraphs, jumps, and labels are the sum of those present
    * in `this` and `other`.
    */
  def ++(other: Cfg): Cfg = {
    if (other == Cfg.empty) {
      this
    } else if (this == Cfg.empty) {
      other
    } else {
      this.copy(
        fringe = other.fringe,
        edges = this.edges ++ other.edges ++
          edgesFromFringeTo(this, other.entryNode),
        jumpsToLabel = this.jumpsToLabel ++ other.jumpsToLabel,
        labeledNodes = this.labeledNodes ++ other.labeledNodes,
        breaks = this.breaks ++ other.breaks,
        continues = this.continues ++ other.continues,
        caseLabels = this.caseLabels ++ other.caseLabels
      )
    }
  }

  def withFringeEdgeType(cfgEdgeType: CfgEdgeType): Cfg = {
    this.copy(fringe = fringe.map { case (x, _) => (x, cfgEdgeType) })
  }

  /** Upon completing traversal of the abstract syntax tree, this method creates CFG edges between jumps like gotos,
    * labeled breaks, labeled continues and respective labels.
    */
  def withResolvedJumpToLabel(): Cfg = {
    val edges = jumpsToLabel.flatMap {
      case (jumpToLabel, label) if label != "*" =>
        labeledNodes.get(label) match {
          case Some(labeledNode) =>
            // TODO set edge type of Always once the backend
            // supports it
            Some(CfgEdge(jumpToLabel, labeledNode, AlwaysEdge))
          case None =>
            logger.info("Unable to wire jump statement. Missing label {}.", label)
            None
        }
      case (jumpToLabel, _) =>
        // We come here for: https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
        // For such GOTOs we cannot statically determine the target label. As a quick
        // hack we simply put edges to all labels found. This might be an over-taint.
        labeledNodes.flatMap { case (_, labeledNode) =>
          Some(CfgEdge(jumpToLabel, labeledNode, AlwaysEdge))
        }
    }
    this.copy(edges = this.edges ++ edges)
  }

}

case class CfgEdge(src: CfgNode, dst: CfgNode, edgeType: CfgEdgeType)

object Cfg {

  private val logger = LoggerFactory.getLogger(getClass)

  def from(cfgs: Cfg*): Cfg = {
    Cfg(
      jumpsToLabel = cfgs.map(_.jumpsToLabel).reduceOption((x, y) => x ++ y).getOrElse(List()),
      breaks = cfgs.map(_.breaks).reduceOption((x, y) => x ++ y).getOrElse(List()),
      continues = cfgs.map(_.continues).reduceOption((x, y) => x ++ y).getOrElse(List()),
      caseLabels = cfgs.map(_.caseLabels).reduceOption((x, y) => x ++ y).getOrElse(List()),
      labeledNodes = cfgs.map(_.labeledNodes).reduceOption((x, y) => x ++ y).getOrElse(Map())
    )
  }

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
    edgesFromFringeTo(cfg.fringe, node)
  }

  /** Create edges from all nodes of cfg's fringe to `node`, ignoring fringe edge types and using `cfgEdgeType` instead.
    */
  def edgesFromFringeTo(cfg: Cfg, node: Option[CfgNode], cfgEdgeType: CfgEdgeType): List[CfgEdge] = {
    edges(cfg.fringe.map(_._1), node, cfgEdgeType)
  }

  /** Create edges from a list (node, cfgEdgeType) pairs to `node`
    */
  def edgesFromFringeTo(fringeElems: List[(CfgNode, CfgEdgeType)], node: Option[CfgNode]): List[CfgEdge] = {
    fringeElems.flatMap { case (sourceNode, cfgEdgeType) =>
      node.map { dstNode =>
        CfgEdge(sourceNode, dstNode, cfgEdgeType)
      }
    }
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

  def takeCurrentLevel(nodesWithLevel: List[(CfgNode, Int)]): List[CfgNode] = {
    nodesWithLevel.collect {
      case (node, level) if level == 1 =>
        node
    }
  }

  def reduceAndFilterLevel(nodesWithLevel: List[(CfgNode, Int)]): List[(CfgNode, Int)] = {
    nodesWithLevel.collect {
      case (node, level) if level != 1 =>
        (node, level - 1)
    }
  }

}
