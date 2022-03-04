package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.SimpleCpgPass
import io.joern.x2cpg.passes.callgraph.MethodRefLinker._
import org.slf4j.{Logger, LoggerFactory}
import overflowdb._
import overflowdb.traversal._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** This pass has MethodStubCreator and TypeDeclStubCreator as prerequisite for language frontends which do not provide
  * method stubs and type decl stubs.
  */
class MethodRefLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {
  import MethodRefLinker.linkToSingle

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // Create REF edges from METHOD_REFs to
    // METHOD

    linkToSingle(
      cpg,
      srcLabels = List(NodeTypes.METHOD_REF),
      dstNodeLabel = NodeTypes.METHOD,
      edgeType = EdgeTypes.REF,
      dstNodeMap = methodFullNameToNode(cpg, _),
      dstFullNameKey = PropertyNames.METHOD_FULL_NAME,
      dstGraph,
      None
    )
  }

}

object MethodRefLinker {
  import overflowdb.BatchedUpdate.DiffGraphBuilder
  val logger: Logger = LoggerFactory.getLogger(classOf[MethodRefLinker])

  def typeDeclFullNameToNode(cpg: Cpg, x: String): Option[TypeDecl] =
    nodesWithFullName(cpg, x).collectFirst { case x: TypeDecl => x }

  def typeFullNameToNode(cpg: Cpg, x: String): Option[Type] =
    nodesWithFullName(cpg, x).collectFirst { case x: Type => x }

  def methodFullNameToNode(cpg: Cpg, x: String): Option[Method] =
    nodesWithFullName(cpg, x).collectFirst { case x: Method => x }

  def namespaceBlockFullNameToNode(cpg: Cpg, x: String): Option[NamespaceBlock] =
    nodesWithFullName(cpg, x).collectFirst { case x: NamespaceBlock => x }

  def nodesWithFullName(cpg: Cpg, x: String): mutable.Seq[NodeRef[_ <: NodeDb]] =
    cpg.graph.indexManager.lookup(PropertyNames.FULL_NAME, x).asScala

  /** For all nodes `n` with a label in `srcLabels`, determine the value of `n.\$dstFullNameKey`, use that to lookup the
    * destination node in `dstNodeMap`, and create an edge of type `edgeType` between `n` and the destination node.
    */
  def linkToSingle(
    cpg: Cpg,
    srcLabels: List[String],
    dstNodeLabel: String,
    edgeType: String,
    dstNodeMap: String => Option[StoredNode],
    dstFullNameKey: String,
    dstGraph: DiffGraphBuilder,
    dstNotExistsHandler: Option[(StoredNode, String) => Unit]
  ): Unit = {
    var loggedDeprecationWarning = false
    Traversal(cpg.graph.nodes(srcLabels: _*)).foreach { srcNode =>
      // If the source node does not have any outgoing edges of this type
      // This check is just required for backward compatibility
      if (srcNode.outE(edgeType).isEmpty) {
        val key = new PropertyKey[String](dstFullNameKey)
        srcNode
          .propertyOption(key)
          .filter { dstFullName =>
            !srcNode.propertyDefaultValue(dstFullNameKey).equals(dstFullName)
          }
          .ifPresent { dstFullName =>
            // for `UNKNOWN` this is not always set, so we're using an Option here
            val srcStoredNode = srcNode.asInstanceOf[StoredNode]
            dstNodeMap(dstFullName) match {
              case Some(dstNode) =>
                dstGraph.addEdge(srcStoredNode, dstNode, edgeType)
              case None if dstNotExistsHandler.isDefined =>
                dstNotExistsHandler.get(srcStoredNode, dstFullName)
              case _ =>
                logFailedDstLookup(edgeType, srcNode.label, srcNode.id.toString, dstNodeLabel, dstFullName)
            }
          }
      } else {
        srcNode.out(edgeType).property(Properties.FULL_NAME).nextOption() match {
          case Some(dstFullName) =>
            dstGraph.setNodeProperty(srcNode.asInstanceOf[StoredNode], dstFullNameKey, dstFullName)
          case None => logger.info(s"Missing outgoing edge of type ${edgeType} from node ${srcNode}")
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
    Traversal(cpg.graph.nodes(srcLabels: _*)).cast[SRC_NODE_TYPE].foreach { srcNode =>
      if (!srcNode.outE(edgeType).hasNext) {
        getDstFullNames(srcNode).foreach { dstFullName =>
          dstNodeMap(dstFullName) match {
            case Some(dstNode) => dstGraph.addEdge(srcNode, dstNode, edgeType)
            case None => logFailedDstLookup(edgeType, srcNode.label, srcNode.id.toString, dstNodeLabel, dstFullName)
          }
        }
      } else {
        val dstFullNames = srcNode.out(edgeType).property(Properties.FULL_NAME).l
        dstGraph.setNodeProperty(srcNode, dstFullNameKey, dstFullNames)
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
  def logFailedDstLookup(
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
  def logFailedSrcLookup(
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
