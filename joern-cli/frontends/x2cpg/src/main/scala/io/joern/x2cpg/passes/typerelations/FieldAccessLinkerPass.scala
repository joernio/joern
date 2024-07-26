package io.joern.x2cpg.passes.typerelations

import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.codepropertygraph.generated.nodes.StoredNode
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.allFieldAccessTypes
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

/** Links field access calls to the field they are accessing to enable the `cpg.fieldAccess.referencedMember` step.
  */
class FieldAccessLinkerPass(cpg: Cpg) extends CpgPass(cpg) with LinkingUtil {

  private val logger = LoggerFactory.getLogger(getClass)
  private val DOT    = "."

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    linkToMultiple(
      cpg,
      srcLabels = List(NodeTypes.CALL),
      dstNodeLabel = NodeTypes.MEMBER,
      edgeType = EdgeTypes.REF,
      dstNodeMap = typeDeclMemberToNode(cpg, _),
      getDstFullNames = (call: Call) => dstMemberFullNames(call),
      dstFullNameKey = PropertyNames.NAME,
      dstGraph
    )
  }

  private def dstMemberFullNames(call: Call): Seq[String] = {
    if (allFieldAccessTypes.contains(call.name)) {
      val fieldAccess = call.asInstanceOf[OpNodes.FieldAccess]
      fieldAccess.argumentOption(1) match
        case Some(baseNode) =>
          fieldAccess.fieldIdentifier.canonicalName.headOption match
            case Some(fieldName) =>
              baseNode.evalType.map(x => s"$x$DOT$fieldName").toSeq
            case None =>
              logger.warn(s"Field access ${fieldAccess.code} has no field identifier")
              Seq.empty
        case None =>
          logger.warn(s"Field access ${fieldAccess.code} has no base node")
          Seq.empty
    } else {
      Seq.empty
    }
  }

  private def typeDeclMemberToNode(cpg: Cpg, fieldFullName: String): Option[Member] = {
    val (typeFullName, fieldName) = fieldFullName.splitAt(fieldFullName.lastIndexOf(DOT))
    typeDeclFullNameToNode(cpg, typeFullName).member.nameExact(fieldName.stripPrefix(DOT)).headOption
  }

  // This is overridden to avoid the step that sets the `dstFullNameKey` property.
  override def linkToMultiple[SRC_NODE_TYPE <: StoredNode](
    cpg: Cpg,
    srcLabels: List[String],
    dstNodeLabel: String,
    edgeType: String,
    dstNodeMap: String => Option[StoredNode],
    getDstFullNames: SRC_NODE_TYPE => Iterable[String],
    dstFullNameKey: String,
    dstGraph: DiffGraphBuilder
  ): Unit = {
    cpg.graph.nodes(srcLabels*).cast[SRC_NODE_TYPE].filterNot(_.outE(edgeType).hasNext).foreach { srcNode =>
      if (!srcNode.outE(edgeType).hasNext) {
        getDstFullNames(srcNode).foreach { dstFullName =>
          dstNodeMap(dstFullName) match {
            case Some(dstNode) =>
              dstGraph.addEdge(srcNode, dstNode, edgeType)
            case None if dstNodeMap(dstFullName).isDefined =>
              dstGraph.addEdge(srcNode, dstNodeMap(dstFullName).get, edgeType)
            case None =>
              logFailedDstLookup(edgeType, srcNode.label, srcNode.id.toString, dstNodeLabel, dstFullName)
          }
        }
      }
    }
  }

}
