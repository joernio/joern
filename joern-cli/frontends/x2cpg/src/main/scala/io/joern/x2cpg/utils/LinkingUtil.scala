package io.joern.x2cpg.utils

import io.joern.x2cpg.passes.frontend.Dereference
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Properties, PropertyNames}
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.traversal.*
import overflowdb.traversal.ChainedImplicitsTemp.*
import overflowdb.{Node, NodeDb, NodeRef, PropertyKey}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

trait LinkingUtil {

  import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

  val MAX_BATCH_SIZE: Int = 100

  val logger: Logger = LoggerFactory.getLogger(classOf[LinkingUtil])

  def typeDeclFullNameToNode(cpg: Cpg, x: String): Option[TypeDecl] =
    nodesWithFullName(cpg, x).collectFirst { case x: TypeDecl => x }

  def typeFullNameToNode(cpg: Cpg, x: String): Option[Type] =
    nodesWithFullName(cpg, x).collectFirst { case x: Type => x }

  def methodFullNameToNode(cpg: Cpg, x: String): Option[Method] =
    nodesWithFullName(cpg, x).collectFirst { case x: Method => x }

  def namespaceBlockFullNameToNode(cpg: Cpg, x: String): Option[NamespaceBlock] =
    nodesWithFullName(cpg, x).collectFirst { case x: NamespaceBlock => x }

  def nodesWithFullName(cpg: Cpg, x: String): mutable.Seq[NodeRef[? <: NodeDb]] =
    cpg.graph.indexManager.lookup(PropertyNames.FULL_NAME, x).asScala

  /** For all nodes `n` with a label in `srcLabels`, determine the value of `n.\$dstFullNameKey`, use that to lookup the
    * destination node in `dstNodeMap`, and create an edge of type `edgeType` between `n` and the destination node.
    */

  protected def linkToSingle(
    cpg: Cpg,
    srcNodes: List[Node],
    srcLabels: List[String],
    dstNodeLabel: String,
    edgeType: String,
    dstNodeMap: String => Option[StoredNode],
    dstFullNameKey: String,
    dstGraph: DiffGraphBuilder,
    dstNotExistsHandler: Option[(StoredNode, String) => Unit]
  ): Unit = {
    val dereference              = Dereference(cpg)
    var loggedDeprecationWarning = false
    srcNodes.foreach { srcNode =>
      // If the source node does not have any outgoing edges of this type
      // This check is just required for backward compatibility
      if (srcNode.outE(edgeType).isEmpty) {
        val key = new PropertyKey[String](dstFullNameKey)
        srcNode
          .propertyOption(key)
          .filter { dstFullName =>
            val dereferenceDstFullName = dereference.dereferenceTypeFullName(dstFullName)
            srcNode.propertyDefaultValue(dstFullNameKey) != dereferenceDstFullName
          }
          .ifPresent { dstFullName =>
            // for `UNKNOWN` this is not always set, so we're using an Option here
            val srcStoredNode          = srcNode.asInstanceOf[StoredNode]
            val dereferenceDstFullName = dereference.dereferenceTypeFullName(dstFullName)
            dstNodeMap(dereferenceDstFullName) match {
              case Some(dstNode) =>
                dstGraph.addEdge(srcStoredNode, dstNode, edgeType)
              case None if dstNodeMap(dstFullName).isDefined =>
                dstGraph.addEdge(srcStoredNode, dstNodeMap(dstFullName).get, edgeType)
              case None if dstNotExistsHandler.isDefined =>
                dstNotExistsHandler.get(srcStoredNode, dereferenceDstFullName)
              case _ =>
                logFailedDstLookup(edgeType, srcNode.label, srcNode.id.toString, dstNodeLabel, dereferenceDstFullName)
            }
          }
      } else {
        srcNode.out(edgeType).property(Properties.FullName).nextOption() match {
          case Some(dstFullName) =>
            dstGraph.setNodeProperty(
              srcNode.asInstanceOf[StoredNode],
              dstFullNameKey,
              dereference.dereferenceTypeFullName(dstFullName)
            )
          case None => logger.info(s"Missing outgoing edge of type $edgeType from node $srcNode")
        }
        if (!loggedDeprecationWarning) {
          logger.info(
            s"Using deprecated CPG format with already existing $edgeType edge between" +
              s" a source node of type $srcLabels and a $dstNodeLabel node."
          )
          loggedDeprecationWarning = true
        }
      }
    }
  }

  def linkToMultiple[SRC_NODE_TYPE <: StoredNode](
    cpg: Cpg,
    srcLabels: List[String],
    dstNodeLabel: String,
    edgeType: String,
    dstNodeMap: String => Option[StoredNode],
    getDstFullNames: SRC_NODE_TYPE => Iterable[String],
    dstFullNameKey: String,
    dstGraph: DiffGraphBuilder
  ): Unit = {
    var loggedDeprecationWarning = false
    val dereference              = Dereference(cpg)
    cpg.graph.nodes(srcLabels*).asScala.cast[SRC_NODE_TYPE].foreach { srcNode =>
      if (!srcNode.outE(edgeType).hasNext) {
        getDstFullNames(srcNode).foreach { dstFullName =>
          val dereferenceDstFullName = dereference.dereferenceTypeFullName(dstFullName)
          dstNodeMap(dereferenceDstFullName) match {
            case Some(dstNode) =>
              dstGraph.addEdge(srcNode, dstNode, edgeType)
            case None if dstNodeMap(dstFullName).isDefined =>
              dstGraph.addEdge(srcNode, dstNodeMap(dstFullName).get, edgeType)
            case None =>
              logFailedDstLookup(edgeType, srcNode.label, srcNode.id.toString, dstNodeLabel, dereferenceDstFullName)
          }
        }
      } else {
        val dstFullNames = srcNode.out(edgeType).property(Properties.FullName).l
        dstGraph.setNodeProperty(srcNode, dstFullNameKey, dstFullNames.map(dereference.dereferenceTypeFullName))
        if (!loggedDeprecationWarning) {
          logger.info(
            s"Using deprecated CPG format with already existing $edgeType edge between" +
              s" a source node of type $srcLabels and a $dstNodeLabel node."
          )
          loggedDeprecationWarning = true
        }
      }
    }
  }

  @inline
  protected def logFailedDstLookup(
    edgeType: String,
    srcNodeType: String,
    srcNodeId: String,
    dstNodeType: String,
    dstFullName: String
  ): Unit = {
    logger.info(
      "Could not create edge. Destination lookup failed. " +
        s"edgeType=$edgeType, srcNodeType=$srcNodeType, srcNodeId=$srcNodeId, " +
        s"dstNodeType=$dstNodeType, dstFullName=$dstFullName"
    )
  }

  @inline
  protected def logFailedSrcLookup(
    edgeType: String,
    srcNodeType: String,
    srcFullName: String,
    dstNodeType: String,
    dstNodeId: String
  ): Unit = {
    logger.info(
      "Could not create edge. Source lookup failed. " +
        s"edgeType=$edgeType, srcNodeType=$srcNodeType, srcFullName=$srcFullName, " +
        s"dstNodeType=$dstNodeType, dstNodeId=$dstNodeId"
    )
  }
}
