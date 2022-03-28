package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.datastructures.OrderTracker
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

    val orderTracker = new OrderTracker()
    Defines.values.foreach { case typeName: Defines.Tpe =>
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
        .order(orderTracker.order)
        .filename("builtintypes")

      diffGraph.addNode(typeDecl)
      orderTracker.inc()
      diffGraph.addEdge(namespaceBlock, typeDecl, EdgeTypes.AST)
    }
  }

}
