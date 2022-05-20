package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewNamespaceBlock, NewType, NewTypeDecl}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes}
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

class BuiltinTypesPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.debug("Generating builtin types.")

    val namespaceBlock = NewNamespaceBlock()
      .name(Defines.GLOBAL_NAMESPACE)
      .fullName(Defines.GLOBAL_NAMESPACE)
      .order(0)
      .filename("builtintypes")

    diffGraph.addNode(namespaceBlock)

    Defines.values.zipWithIndex.map { case (typeName: Defines.Tpe, index) =>
      val typeNameLabel = typeName.label

      val tpe = NewType()
        .name(typeNameLabel)
        .fullName(typeNameLabel)
        .typeDeclFullName(typeNameLabel)
      diffGraph.addNode(tpe)

      val typeDecl = NewTypeDecl()
        .name(typeNameLabel)
        .fullName(typeNameLabel)
        .isExternal(false)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(Defines.GLOBAL_NAMESPACE)
        .order(index + 1)
        .filename("builtintypes")

      diffGraph.addNode(typeDecl)
      diffGraph.addEdge(namespaceBlock, typeDecl, EdgeTypes.AST)
    }
  }

}
