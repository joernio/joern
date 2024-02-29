package io.joern.x2cpg.passes.base

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.Node
import overflowdb.traversal.*

class TypeUsagePassTwo(cpg: Cpg) extends ForkJoinParallelCpgPass[List[Node]](cpg) with LinkingUtil {
  val srcLabels = List(
    NodeTypes.METHOD_PARAMETER_IN,
    NodeTypes.METHOD_PARAMETER_OUT,
    NodeTypes.METHOD_RETURN,
    NodeTypes.MEMBER,
    NodeTypes.LITERAL,
    NodeTypes.CALL,
    NodeTypes.LOCAL,
    NodeTypes.IDENTIFIER,
    NodeTypes.BLOCK,
    NodeTypes.METHOD_REF,
    NodeTypes.TYPE_REF,
    NodeTypes.UNKNOWN
  )

  def generateParts(): Array[List[Node]] = cpg.graph.nodes(srcLabels: _*).toList.grouped(BATCH_SIZE).toArray
  def runOnPart(builder: DiffGraphBuilder, part: List[overflowdb.Node]): Unit = {
    newLinkToSingle(
      cpg = cpg,
      srcNodes = part,
      srcLabels = srcLabels,
      dstNodeLabel = NodeTypes.TYPE,
      edgeType = EdgeTypes.EVAL_TYPE,
      dstNodeMap = typeFullNameToNode(cpg, _),
      dstFullNameKey = "TYPE_FULL_NAME",
      dstGraph = builder,
      None
    )
  }
}
