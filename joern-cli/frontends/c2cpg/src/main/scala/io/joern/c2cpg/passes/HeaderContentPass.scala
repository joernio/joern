package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.datastructures.CGlobal
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.utils.IncludeAutoDiscovery
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes, Properties, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.Ast
import overflowdb.traversal._

class HeaderContentPass(cpg: Cpg, config: Config) extends SimpleCpgPass(cpg) {

  private val systemIncludePaths =
    IncludeAutoDiscovery.discoverIncludePathsC(config) ++ IncludeAutoDiscovery.discoverIncludePathsCPP(config)

  private val absolutePath: String = File(config.inputPath).path.toAbsolutePath.normalize().toString
  private val filename: String     = s"$absolutePath:<includes>"
  private val globalName: String   = NamespaceTraversal.globalNamespaceName
  private val fullName: String     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))

  private val typeDeclFullNames: Set[String] =
    cpg.graph.nodes(NodeTypes.TYPE_DECL).map(_.property(Properties.FULL_NAME)).toSetImmutable

  private def setExternal(node: HasFilename, diffGraph: DiffGraphBuilder): Unit = {
    if (node.isInstanceOf[HasIsExternal] && systemIncludePaths.exists(p => node.filename.startsWith(p.toString))) {
      diffGraph.setNodeProperty(node.asInstanceOf[StoredNode], PropertyNames.IS_EXTERNAL, Boolean.box(true))
    }
  }

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

    val blockNode = NewBlock()
      .typeFullName("ANY")

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")

    val ast = Ast(includesFile).withChild(
      Ast(namespaceBlock)
        .withChild(Ast(fakeGlobalIncludesMethod).withChild(Ast(blockNode)).withChild(Ast(methodReturn)))
    )

    Ast.storeInDiffGraph(ast, dstGraph)
    blockNode
  }

  private def createMissingAstEdges(dstGraph: DiffGraphBuilder): Unit = {
    val globalBlock = createGlobalBlock(dstGraph)
    Traversal(cpg.graph.nodes()).whereNot(_.inE(EdgeTypes.AST)).foreach {
      case srcNode: HasFilename if FileDefaults.isHeaderFile(srcNode.filename) =>
        dstGraph.addEdge(globalBlock, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
        setExternal(srcNode, dstGraph)
      case srcNode: Local =>
        dstGraph.addEdge(globalBlock, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
      case _ =>
    }
  }

  private def typeNeedsTypeDeclStub(t: Type): Boolean =
    !typeDeclFullNames.contains(t.typeDeclFullName)

  private def createMissingTypeDecls(dstGraph: DiffGraphBuilder): Unit = {
    Traversal(cpg.graph.nodes(NodeTypes.TYPE))
      .foreach {
        case t: Type if typeNeedsTypeDeclStub(t) =>
          val newTypeDecl = NewTypeDecl()
            .name(t.name)
            .fullName(t.typeDeclFullName)
            .code(t.name)
            .isExternal(true)
            .filename(filename)
            .astParentType(NodeTypes.NAMESPACE_BLOCK)
            .astParentFullName(fullName)
          Ast.storeInDiffGraph(Ast(newTypeDecl), dstGraph)
        case _ => // we are fine
      }
  }

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    if (!CGlobal.shouldBeCleared()) {
      ;
    } else {
      createMissingAstEdges(dstGraph)
      createMissingTypeDecls(dstGraph)
    }
  }

}
