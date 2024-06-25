package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewType, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.passes.CpgPass

class BuiltinTypesPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val namespaceBlock = NewNamespaceBlock()
      .name(Defines.GlobalNamespace)
      .fullName(Defines.GlobalNamespace)
      .order(0)
      .filename("builtintypes")

    diffGraph.addNode(namespaceBlock)

    Defines.SwiftTypes.zipWithIndex.map { case (typeName: String, index) =>
      val tpe = NewType()
        .name(typeName)
        .fullName(typeName)
        .typeDeclFullName(typeName)
      diffGraph.addNode(tpe)

      val typeDecl = NewTypeDecl()
        .name(typeName)
        .fullName(typeName)
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
