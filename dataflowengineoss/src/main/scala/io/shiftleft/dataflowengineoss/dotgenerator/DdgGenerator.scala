package io.shiftleft.dataflowengineoss.dotgenerator

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Properties}
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.utils.MemberAccess.isGenericMemberAccessName
import overflowdb.Node
import overflowdb.traversal.jIteratortoTraversal

import scala.collection.mutable

class DdgGenerator {

  val edgeType = "DDG"
  private val edgeCache = mutable.Map[StoredNode, List[Edge]]()

  def generate(methodNode: Method)(implicit semantics: Semantics): Graph = {
    val entryNode = methodNode
    val paramNodes = methodNode.parameter.l
    val allOtherNodes = methodNode.cfgNode.l
    val exitNode = methodNode.methodReturn
    val allNodes: List[StoredNode] = List(entryNode, exitNode) ++ paramNodes ++ allOtherNodes
    val visibleNodes = allNodes.filter(shouldBeDisplayed)

    val edges = visibleNodes.map { dstNode =>
      inEdgesToDisplay(dstNode)
    }

    val allIdsReferencedByEdges = edges.flatten.flatMap { edge: Edge =>
      Set(edge.src.id, edge.dst.id)
    }

    val ddgNodes = visibleNodes
      .filter(node => allIdsReferencedByEdges.contains(node.id))
      .map(surroundingCall)
      .filterNot(node => node.isInstanceOf[Call] && isGenericMemberAccessName(node.asInstanceOf[Call].name))

    val ddgEdges = edges.flatten
      .map { e: Edge =>
        e.copy(src = surroundingCall(e.src), dst = surroundingCall(e.dst))
      }
      .filter(e => e.src != e.dst)
      .filterNot(e => e.dst.isInstanceOf[Call] && isGenericMemberAccessName(e.dst.asInstanceOf[Call].name))
      .filterNot(e => e.src.isInstanceOf[Call] && isGenericMemberAccessName(e.src.asInstanceOf[Call].name))
      .distinct

    edgeCache.clear()
    Graph(ddgNodes, ddgEdges)
  }

  private def surroundingCall(node: StoredNode): StoredNode = {
    node match {
      case arg: Expression => arg.inCall.headOption.getOrElse(node)
      case _               => node
    }
  }

  private def shouldBeDisplayed(v: Node): Boolean = !(
    v.isInstanceOf[Block] ||
      v.isInstanceOf[ControlStructure] ||
      v.isInstanceOf[JumpTarget]
  )

  private def inEdgesToDisplay(dstNode: StoredNode, visited: List[StoredNode] = List())(
      implicit semantics: Semantics): List[Edge] = {

    if (edgeCache.contains(dstNode)) {
      return edgeCache(dstNode)
    }

    if (visited.contains(dstNode)) {
      List()
    } else {
      val parents = expand(dstNode)
      val (visible, invisible) = parents.partition(x => shouldBeDisplayed(x.src) && x.srcVisible)
      val result = visible.toList ++ invisible.toList.flatMap { n =>
        val parentInEdgesToDisplay = inEdgesToDisplay(n.src, visited ++ List(dstNode))
        parentInEdgesToDisplay.map(y => Edge(y.src, dstNode, y.srcVisible, edgeType = edgeType, label = y.label))
      }.distinct
      edgeCache.put(dstNode, result)
      result
    }
  }

  private def expand(v: StoredNode)(implicit semantics: Semantics): Iterator[Edge] = {

    val allInEdges = v
      .inE(EdgeTypes.REACHING_DEF)
      .map(x => Edge(x.outNode.asInstanceOf[StoredNode], v, true, x.property(Properties.VARIABLE), edgeType))

    v match {
      case cfgNode: CfgNode =>
        cfgNode
          .ddgInPathElem(withInvisible = true)
          .map(x => Edge(x.node.asInstanceOf[StoredNode], v, x.visible, x.outEdgeLabel, edgeType))
          .iterator ++ allInEdges.filter(_.src.isInstanceOf[Method]).iterator
      case _ =>
        allInEdges.iterator
    }

  }

}
