package io.joern.x2cpg.passes.typerelations

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.joern.x2cpg.passes.callgraph.MethodRefLinker.{linkToMultiple, typeFullNameToNode}

/** Create INHERITS_FROM edges from `TYPE_DECL` nodes to `TYPE` nodes.
  */
class TypeHierarchyPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

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
