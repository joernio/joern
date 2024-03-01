package io.joern.x2cpg.passes.base

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.Node
import overflowdb.traversal.*

class TypeRefPass(cpg: Cpg) extends ForkJoinParallelCpgPass[List[Node]](cpg) with LinkingUtil {
  val srcLabels = List(NodeTypes.TYPE)

  def generateParts(): Array[List[Node]] = {
    val nodes = cpg.graph.nodes(srcLabels: _*).toList
    nodes.grouped(getBatchSize(nodes.size)).toArray
  }
  def runOnPart(builder: DiffGraphBuilder, part: List[overflowdb.Node]): Unit = {
    linkToSingle(
      cpg = cpg,
      srcNodes = part,
      srcLabels = srcLabels,
      dstNodeLabel = NodeTypes.TYPE_DECL,
      edgeType = EdgeTypes.REF,
      dstNodeMap = typeDeclFullNameToNode(cpg, _),
      dstFullNameKey = PropertyNames.TYPE_DECL_FULL_NAME,
      dstGraph = builder,
      None
    )
  }
}
