package io.joern.x2cpg.passes.callgraph

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2._
import io.shiftleft.codepropertygraph.generated.v2.nodes.Method
import io.shiftleft.passes.CpgPass
import io.joern.x2cpg.utils.LinkingUtil

/** This pass has MethodStubCreator and TypeDeclStubCreator as prerequisite for language frontends which do not provide
  * method stubs and type decl stubs.
  */
class MethodRefLinker(cpg: Cpg) extends CpgPass(cpg) with LinkingUtil {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    // Create REF edges from METHOD_REFs to METHOD
    linkToSingle(
      cpg,
      srcLabels = List(NodeTypes.METHOD_REF),
      dstNodeLabel = NodeTypes.METHOD,
      edgeType = EdgeTypes.REF,
      dstNodeMap = methodFullNameToNode(cpg, _),
      dstFullNameKey = PropertyKeys.MethodFullName,
      dstDefaultPropertyValue = Method.PropertyDefaults.FullName,
      dstGraph,
      None
    )
  }

}
