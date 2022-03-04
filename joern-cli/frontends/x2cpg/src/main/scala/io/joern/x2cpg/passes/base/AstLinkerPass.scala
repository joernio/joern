package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{HasAstParentFullName, HasAstParentType, StoredNode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Properties}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.callgraph.MethodRefLinker
import io.joern.x2cpg.passes.callgraph.MethodRefLinker.{
  methodFullNameToNode,
  namespaceBlockFullNameToNode,
  typeDeclFullNameToNode
}
import overflowdb.traversal._

class AstLinkerPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  import MethodRefLinker.{logFailedSrcLookup, logger}

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    cpg.method.whereNot(_.inE(EdgeTypes.AST)).foreach(addAstEdge(_, dstGraph))
    cpg.typeDecl.whereNot(_.inE(EdgeTypes.AST)).foreach(addAstEdge(_, dstGraph))
  }

  /** For the given method or type declaration, determine its parent in the AST via the AST_PARENT_TYPE and
    * AST_PARENT_FULL_NAME fields and create an AST edge from the parent to it. AST creation to methods and type
    * declarations is deferred in frontends in order to allow them to process methods/type- declarations independently.
    */
  private def addAstEdge(
    methodOrTypeDecl: HasAstParentType with HasAstParentFullName with StoredNode,
    dstGraph: DiffGraphBuilder
  ): Unit = {
    val astParentOption: Option[StoredNode] =
      methodOrTypeDecl.astParentType match {
        case NodeTypes.METHOD          => methodFullNameToNode(cpg, methodOrTypeDecl.astParentFullName)
        case NodeTypes.TYPE_DECL       => typeDeclFullNameToNode(cpg, methodOrTypeDecl.astParentFullName)
        case NodeTypes.NAMESPACE_BLOCK => namespaceBlockFullNameToNode(cpg, methodOrTypeDecl.astParentFullName)
        case _ =>
          logger.warn(
            s"Invalid AST_PARENT_TYPE=${methodOrTypeDecl.propertyOption(Properties.AST_PARENT_FULL_NAME)};" +
              s" astChild LABEL=${methodOrTypeDecl.label};" +
              s" astChild FULL_NAME=${methodOrTypeDecl.propertyOption(Properties.FULL_NAME)}"
          )
          None
      }

    astParentOption match {
      case Some(astParent) =>
        dstGraph.addEdge(astParent, methodOrTypeDecl, EdgeTypes.AST)
      case None =>
        logFailedSrcLookup(
          EdgeTypes.AST,
          methodOrTypeDecl.astParentType,
          methodOrTypeDecl.astParentFullName,
          methodOrTypeDecl.label,
          methodOrTypeDecl.id.toString
        )
    }
  }
}
