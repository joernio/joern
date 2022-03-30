package io.shiftleft.semanticcpg.dotgenerator

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.dotgenerator.DotSerializer.{Edge, Graph}
import io.shiftleft.semanticcpg.language._
import overflowdb.Node

import scala.jdk.CollectionConverters._

class CfgGenerator {

  val edgeType: String = EdgeTypes.CFG

  def generate(methodNode: Method): Graph = {
    val vertices          = methodNode.cfgNode.l ++ List(methodNode, methodNode.methodReturn) ++ methodNode.parameter.l
    val verticesToDisplay = vertices.filter(cfgNodeShouldBeDisplayed)

    def edgesToDisplay(srcNode: StoredNode, visited: List[StoredNode] = List()): List[Edge] = {
      if (visited.contains(srcNode)) {
        List()
      } else {
        val children             = expand(srcNode).filter(x => vertices.contains(x.dst))
        val (visible, invisible) = children.partition(x => cfgNodeShouldBeDisplayed(x.dst))
        visible.toList ++ invisible.toList.flatMap { n =>
          edgesToDisplay(n.dst, visited ++ List(srcNode)).map(y => Edge(srcNode, y.dst, edgeType = edgeType))
        }
      }
    }

    val edges = verticesToDisplay.flatMap { v =>
      edgesToDisplay(v)
    }.distinct

    val allIdsReferencedByEdges = edges.flatMap { edge =>
      Set(edge.src.id, edge.dst.id)
    }

    Graph(
      verticesToDisplay
        .filter(node => allIdsReferencedByEdges.contains(node.id)),
      edges
    )
  }

  protected def expand(v: StoredNode): Iterator[Edge] = {
    v._cfgOut.asScala
      .filter(_.isInstanceOf[StoredNode])
      .map(node => Edge(v, node, edgeType = edgeType))
  }

  def cfgNodeShouldBeDisplayed(v: Node): Boolean = !(
    v.isInstanceOf[Literal] ||
      v.isInstanceOf[Identifier] ||
      v.isInstanceOf[Block] ||
      v.isInstanceOf[ControlStructure] ||
      v.isInstanceOf[JumpTarget] ||
      v.isInstanceOf[MethodParameterIn]
  )

}
