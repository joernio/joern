package io.joern.x2cpg.passes.base

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Type, StoredNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class TypeRefPass(cpg: Cpg) extends ForkJoinParallelCpgPass[List[StoredNode]](cpg) with LinkingUtil {
  private val srcLabels = List(NodeTypes.TYPE)

  def generateParts(): Array[List[StoredNode]] = {
    cpg.graph.nodes(srcLabels*).cast[StoredNode].toList.grouped(MAX_BATCH_SIZE).toArray
  }

  def runOnPart(builder: DiffGraphBuilder, part: List[StoredNode]): Unit = {
    linkToSingle(
      cpg = cpg,
      srcNodes = part,
      srcLabels = srcLabels,
      dstNodeLabel = NodeTypes.TYPE_DECL,
      edgeType = EdgeTypes.REF,
      dstNodeMap = typeDeclFullNameToNode(cpg, _),
      dstFullNameKey = PropertyNames.TYPE_DECL_FULL_NAME,
      dstDefaultPropertyValue = Type.PropertyDefaults.TypeDeclFullName,
      dstGraph = builder,
      None
    )
  }
}
