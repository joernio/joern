package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.joern.x2cpg.utils.LinkingUtil

class AliasLinkerPass(cpg: Cpg) extends CpgPass(cpg) with LinkingUtil {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // Create ALIAS_OF edges from TYPE_DECL nodes to TYPE
    linkToMultiple(
      cpg,
      srcLabels = List(NodeTypes.TYPE_DECL),
      dstNodeLabel = NodeTypes.TYPE,
      edgeType = EdgeTypes.ALIAS_OF,
      dstNodeMap = typeFullNameToNode(cpg, _),
      getDstFullNames = (srcNode: TypeDecl) => {
        srcNode.aliasTypeFullName
      },
      dstFullNameKey = PropertyNames.ALIAS_TYPE_FULL_NAME,
      dstGraph
    )
  }

}
