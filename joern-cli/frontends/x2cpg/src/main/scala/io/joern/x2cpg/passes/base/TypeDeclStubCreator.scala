package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewTypeDecl, TypeDeclBase}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.{FileTraversal, NamespaceTraversal}

/** This pass has no other pass as prerequisite. For each `TYPE` node that does not have a corresponding `TYPE_DECL`
  * node, this pass creates a `TYPE_DECL` node. The `TYPE_DECL` is considered external.
  */
class TypeDeclStubCreator(cpg: Cpg) extends CpgPass(cpg) {

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
        val newTypeDecl = TypeDeclStubCreator.createTypeDeclStub(typ.name, typ.fullName)
        typeDeclFullNameToNode += typ.fullName -> newTypeDecl
        dstGraph.addNode(newTypeDecl)
      }
  }

}

object TypeDeclStubCreator {

  def createTypeDeclStub(
    name: String,
    fullName: String,
    isExternal: Boolean = true,
    astParentType: String = NodeTypes.NAMESPACE_BLOCK,
    astParentFullName: String = NamespaceTraversal.globalNamespaceName,
    fileName: String = FileTraversal.UNKNOWN
  ): NewTypeDecl = {
    NewTypeDecl()
      .name(name)
      .fullName(fullName)
      .isExternal(isExternal)
      .inheritsFromTypeFullName(IndexedSeq.empty)
      .astParentType(astParentType)
      .astParentFullName(astParentFullName)
      .filename(fileName)
  }

}
