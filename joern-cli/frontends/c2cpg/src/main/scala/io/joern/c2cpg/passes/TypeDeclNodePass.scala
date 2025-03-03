package io.joern.c2cpg.passes

import io.joern.c2cpg.astcreation.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode

class TypeDeclNodePass(cpg: Cpg)(implicit withSchemaValidation: ValidationMode) extends CpgPass(cpg) {

  private val filename: String               = "<includes>"
  private val globalName: String             = NamespaceTraversal.globalNamespaceName
  private val fullName: String               = MetaDataPass.getGlobalNamespaceBlockFullName(Option(filename))
  private val typeDeclFullNames: Set[String] = cpg.typeDecl.fullName.toSetImmutable

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    var hadMissingTypeDecl = false
    cpg.typ.filter(typeNeedsTypeDeclStub).foreach { t =>
      val newTypeDecl = NewTypeDecl()
        .name(t.name)
        .fullName(t.typeDeclFullName)
        .code(t.name)
        .isExternal(true)
        .filename(filename)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(fullName)
      dstGraph.addNode(newTypeDecl)
      hadMissingTypeDecl = true
    }
    if (hadMissingTypeDecl) Ast.storeInDiffGraph(createGlobalAst(), dstGraph)
  }

  private def createGlobalAst(): Ast = {
    val includesFile = NewFile().name(filename)
    val namespaceBlock = NewNamespaceBlock()
      .name(globalName)
      .fullName(fullName)
      .filename(filename)
    val fakeGlobalIncludesMethod =
      NewMethod()
        .name(globalName)
        .code(globalName)
        .fullName(fullName)
        .filename(filename)
        .lineNumber(1)
        .astParentType(NodeTypes.NAMESPACE_BLOCK)
        .astParentFullName(fullName)
    val blockNode    = NewBlock().typeFullName(Defines.Any)
    val methodReturn = newMethodReturnNode(Defines.Any, line = None, column = None)
    Ast(includesFile).withChild(
      Ast(namespaceBlock)
        .withChild(Ast(fakeGlobalIncludesMethod).withChild(Ast(blockNode)).withChild(Ast(methodReturn)))
    )
  }

  private def typeNeedsTypeDeclStub(t: Type): Boolean = {
    !typeDeclFullNames.contains(t.typeDeclFullName)
  }

}
