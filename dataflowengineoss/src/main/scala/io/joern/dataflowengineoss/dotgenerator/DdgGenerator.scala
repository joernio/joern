package io.joern.dataflowengineoss.dotgenerator

import io.joern.dataflowengineoss.DefaultSemantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.MemberAccess.isGenericMemberAccessName

import scala.collection.mutable

class DdgGenerator {

  val edgeType          = "DDG"
  private val edgeCache = mutable.Map[StoredNode, List[Edge]]()

  def generate(methodNode: Method)(implicit semantics: Semantics = DefaultSemantics()): Graph = {
    val entryNode                  = methodNode
    val paramNodes                 = methodNode.parameter.l
    val allOtherNodes              = methodNode.cfgNode.l
    val exitNode                   = methodNode.methodReturn
    val allNodes: List[StoredNode] = List(entryNode, exitNode) ++ paramNodes ++ allOtherNodes
    val visibleNodes               = allNodes.filter(shouldBeDisplayed)

    val edges = visibleNodes.map { dstNode =>
      inEdgesToDisplay(dstNode)
    }

    val allIdsReferencedByEdges = edges.flatten.flatMap { edge =>
      Set(edge.src.id, edge.dst.id)
    }

    val ddgNodes = visibleNodes
      .filter(node => allIdsReferencedByEdges.contains(node.id))
      .map(surroundingCall)
      .filterNot(node => node.isInstanceOf[Call] && isGenericMemberAccessName(node.asInstanceOf[Call].name))

    val ddgEdges = edges.flatten
      .map { edge =>
        edge.copy(src = surroundingCall(edge.src), dst = surroundingCall(edge.dst))
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

  private def shouldBeDisplayed(v: StoredNode): Boolean = !(
    v.isInstanceOf[ControlStructure] ||
      v.isInstanceOf[JumpTarget]
  )

  private def inEdgesToDisplay(dstNode: StoredNode, visited: List[StoredNode] = List())(implicit
    semantics: Semantics
  ): List[Edge] = {

    if (edgeCache.contains(dstNode)) {
      return edgeCache(dstNode)
    }

    if (visited.contains(dstNode)) {
      List()
    } else {
      val parents              = expand(dstNode)
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
      .map(x =>
        // note: this looks strange, but let me explain...
        // in overflowdb, edges were allowed multiple properties and this used to be `x.property(Properties.VARIABLE)`
        // in flatgraph an edge may have zero or one properties and they're not named...
        // in this case we know that we're dealing with ReachingDef edges which has the `variable` property
        val variablePropertyMaybe = x.property match {
          case null                     => null
          case variableProperty: String => variableProperty
          case _                        => null
        }
        Edge(x.src.asInstanceOf[StoredNode], v, srcVisible = true, variablePropertyMaybe, edgeType)
      )

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
