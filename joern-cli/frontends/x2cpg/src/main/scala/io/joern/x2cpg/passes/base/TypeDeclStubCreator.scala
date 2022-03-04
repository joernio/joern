package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewTypeDecl, TypeDeclBase}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.{FileTraversal, NamespaceTraversal}

/** This pass has no other pass as prerequisite. For each `TYPE` node that does not have a corresponding `TYPE_DECL`
  * node, this pass creates a `TYPE_DECL` node. The `TYPE_DECL` is considered external.
  */
class TypeDeclStubCreator(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private var typeDeclFullNameToNode = Map[String, TypeDeclBase]()

  private def privateInit(): Unit = {
    cpg.typeDecl
      .foreach { typeDecl =>
        typeDeclFullNameToNode += typeDecl.fullName -> typeDecl
      }
  }

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    privateInit()

    cpg.typ
      .filterNot(typ => typeDeclFullNameToNode.isDefinedAt(typ.fullName))
      .foreach { typ =>
        val newTypeDecl = createTypeDeclStub(typ.name, typ.fullName)
        typeDeclFullNameToNode += typ.fullName -> newTypeDecl
        dstGraph.addNode(newTypeDecl)
      }
  }

  private def createTypeDeclStub(name: String, fullName: String): NewTypeDecl = {
    NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .isExternal(true)
      .inheritsFromTypeFullName(IndexedSeq.empty)
      .astParentType(NodeTypes.NAMESPACE_BLOCK)
      .astParentFullName(NamespaceTraversal.globalNamespaceName)
      .filename(FileTraversal.UNKNOWN)
  }

}
