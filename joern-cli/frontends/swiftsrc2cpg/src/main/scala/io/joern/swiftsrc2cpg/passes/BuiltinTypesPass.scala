package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.passes.frontend.TypeNodePass
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.passes.CpgPass

class BuiltinTypesPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val namespaceBlock = NewNamespaceBlock()
      .name(Defines.GlobalNamespace)
      .fullName(Defines.GlobalNamespace)
      .order(0)
      .filename("builtintypes")

    diffGraph.addNode(namespaceBlock)

    Defines.SwiftTypes.diff(Seq(Defines.Any)).zipWithIndex.map { case (fullName: String, index) =>
      val name = TypeNodePass.fullToShortName(fullName)
      val typeDecl = NewTypeDecl()
        .name(name)
        .fullName(fullName)
        .isExternal(true)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(Defines.GlobalNamespace)
        .order(index + 1)
        .filename("builtintypes")
      diffGraph.addNode(typeDecl)
      diffGraph.addEdge(namespaceBlock, typeDecl, EdgeTypes.AST)
    }
  }

}
