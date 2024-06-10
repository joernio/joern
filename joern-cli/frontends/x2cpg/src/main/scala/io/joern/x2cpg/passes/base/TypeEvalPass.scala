package io.joern.x2cpg.passes.base

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.Node
import overflowdb.traversal.*

class TypeEvalPass(cpg: Cpg) extends ForkJoinParallelCpgPass[List[Node]](cpg) with LinkingUtil {

  private val srcLabels = List(
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

  def generateParts(): Array[List[Node]] = {
    cpg.graph.nodes(srcLabels*).toList.grouped(MAX_BATCH_SIZE).toArray
  }

  def runOnPart(builder: DiffGraphBuilder, part: List[overflowdb.Node]): Unit = {
    linkToSingle(
      cpg = cpg,
      srcNodes = part,
      srcLabels = srcLabels,
      dstNodeLabel = NodeTypes.TYPE,
      edgeType = EdgeTypes.EVAL_TYPE,
      dstNodeMap = typeFullNameToNode(cpg, _),
      dstFullNameKey = PropertyNames.TYPE_FULL_NAME,
      dstGraph = builder,
      None
    )
  }
}
