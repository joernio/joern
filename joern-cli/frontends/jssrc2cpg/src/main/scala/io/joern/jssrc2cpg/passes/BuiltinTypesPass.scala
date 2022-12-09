package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewType, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.passes.CpgPass

class BuiltinTypesPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val namespaceBlock = NewNamespaceBlock()
      .name(Defines.GLOBAL_NAMESPACE)
      .fullName(Defines.GLOBAL_NAMESPACE)
      .order(0)
      .filename("builtintypes")

    diffGraph.addNode(namespaceBlock)

    Defines.JSTYPES.zipWithIndex.map { case (typeName: String, index) =>
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
        .astParentFullName(Defines.GLOBAL_NAMESPACE)
        .order(index + 1)
        .filename("builtintypes")

      diffGraph.addNode(typeDecl)
      diffGraph.addEdge(namespaceBlock, typeDecl, EdgeTypes.AST)
    }
  }

}
