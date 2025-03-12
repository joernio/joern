package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Create INHERITS_FROM edges from `TYPE_DECL` nodes to `TYPE` nodes.
  */
class TypeHierarchyPass(cpg: Cpg) extends CpgPass(cpg) with LinkingUtil {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    linkToMultiple(
      cpg,
      srcLabels = List(NodeTypes.TYPE_DECL),
      dstNodeLabel = NodeTypes.TYPE,
      edgeType = EdgeTypes.INHERITS_FROM,
      dstNodeMap = typeFullNameToNode(cpg, _),
      getDstFullNames = (srcNode: TypeDecl) => {
        if (srcNode.inheritsFromTypeFullName != null) {
          srcNode.inheritsFromTypeFullName
        } else {
          Seq()
        }
      },
      dstFullNameKey = PropertyNames.INHERITS_FROM_TYPE_FULL_NAME,
      dstGraph
    )
  }

}
