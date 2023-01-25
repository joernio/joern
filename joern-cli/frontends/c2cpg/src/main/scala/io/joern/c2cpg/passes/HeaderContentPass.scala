package io.joern.c2cpg.passes

import io.joern.c2cpg.Config
import io.joern.c2cpg.astcreation.Defines
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.utils.IncludeAutoDiscovery
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.methodReturnNode
import overflowdb.traversal.toNodeTraversal

class HeaderContentPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  private val systemIncludePaths =
    IncludeAutoDiscovery.discoverIncludePathsC(config) ++ IncludeAutoDiscovery.discoverIncludePathsCPP(config)

  private val filename: String   = "<includes>"
  private val globalName: String = NamespaceTraversal.globalNamespaceName
  private val fullName: String   = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))

  private val typeDeclFullNames: Set[String] = cpg.typeDecl.fullName.toSetImmutable

  private def createGlobalBlock(dstGraph: DiffGraphBuilder): NewBlock = {
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

    val blockNode = NewBlock().typeFullName(Defines.anyTypeName)

    val methodReturn = methodReturnNode(Defines.anyTypeName, line = None, column = None)

    val ast = Ast(includesFile).withChild(
      Ast(namespaceBlock)
        .withChild(Ast(fakeGlobalIncludesMethod).withChild(Ast(blockNode)).withChild(Ast(methodReturn)))
    )

    Ast.storeInDiffGraph(ast, dstGraph)
    blockNode
  }

  private def createMissingAstEdges(dstGraph: DiffGraphBuilder): Unit = {
    val globalBlock = createGlobalBlock(dstGraph)
    cpg.all.not(_.inE(EdgeTypes.AST)).foreach {
      case srcNode: Method =>
        dstGraph.addEdge(globalBlock, srcNode, EdgeTypes.AST)
        if (systemIncludePaths.exists(p => srcNode.filename.startsWith(p.toString))) {
          dstGraph.setNodeProperty(srcNode, PropertyNames.IS_EXTERNAL, true)
        }
      case srcNode: TypeDecl =>
        dstGraph.addEdge(globalBlock, srcNode, EdgeTypes.AST)
        if (systemIncludePaths.exists(p => srcNode.filename.startsWith(p.toString))) {
          dstGraph.setNodeProperty(srcNode, PropertyNames.IS_EXTERNAL, true)
        }
      case _ @(_: MetaData | _: Binding | _: Type) => // do nothing
      case srcNode =>
        dstGraph.addEdge(globalBlock, srcNode, EdgeTypes.AST)
    }
  }

  private def typeNeedsTypeDeclStub(t: Type): Boolean =
    !typeDeclFullNames.contains(t.typeDeclFullName)

  private def createMissingTypeDecls(dstGraph: DiffGraphBuilder): Unit = {
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
    }
  }

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    if (CGlobal.shouldBeCleared()) {
      createMissingAstEdges(dstGraph)
      createMissingTypeDecls(dstGraph)
    }
  }

}
