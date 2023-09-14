package io.joern.x2cpg.passes.base

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes.{Local, StoredNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class TypeEvalPass(cpg: Cpg) extends ForkJoinParallelCpgPass[List[StoredNode]](cpg) with LinkingUtil {
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

  def generateParts(): Array[List[StoredNode]] = {
    cpg.graph.nodes(srcLabels*).cast[StoredNode].toList.grouped(MAX_BATCH_SIZE).toArray
  }

  def runOnPart(builder: DiffGraphBuilder, part: List[StoredNode]): Unit = {
    linkToSingle(
      cpg = cpg,
      srcNodes = part,
      srcLabels = srcLabels,
      dstNodeLabel = NodeTypes.TYPE,
      edgeType = EdgeTypes.EVAL_TYPE,
      dstNodeMap = typeFullNameToNode(cpg, _),
      dstFullNameKey = PropertyNames.TYPE_FULL_NAME,
      dstDefaultPropertyValue = Local.PropertyDefaults.TypeFullName,
      dstGraph = builder,
      None
    )
  }
}
