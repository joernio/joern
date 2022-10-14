package io.joern.x2cpg.passes.callgraph

import io.joern.x2cpg.passes.EdgeLinker.{linkToSingle, methodFullNameToNode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.passes.SimpleCpgPass

/** This pass has MethodStubCreator and TypeDeclStubCreator as prerequisite for language frontends which do not provide
  * method stubs and type decl stubs.
  */
class MethodRefLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

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
