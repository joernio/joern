package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.joern.x2cpg.passes.callgraph.MethodRefLinker.{linkToMultiple, typeFullNameToNode}

class AliasLinkerPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // Create ALIAS_OF edges from TYPE_DECL nodes to
    // TYPE
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
