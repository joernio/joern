package io.joern.c2cpg.passes

import better.files.File
import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.datastructures.Global
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.utils.IncludeAutoDiscovery
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes, Properties, PropertyNames}
import io.shiftleft.passes.{CpgPass, DiffGraph, KeyPool}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.x2cpg.Ast
import overflowdb.traversal._

class HeaderContentPass(cpg: Cpg, keyPool: Option[KeyPool], config: Config) extends CpgPass(cpg, keyPool = keyPool) {

  private val systemIncludePaths = IncludeAutoDiscovery.discoverIncludePaths(config)

  private val absolutePath: String = File(config.inputPaths.head).path.toAbsolutePath.normalize().toString
  private val filename: String     = s"$absolutePath:<includes>"
  private val globalName: String   = NamespaceTraversal.globalNamespaceName
  private val fullName: String     = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))

  private val typeDeclFullNames: Set[String] =
    cpg.graph.nodes(NodeTypes.TYPE_DECL).map(_.property(Properties.FULL_NAME)).toSetImmutable

  private def setExternal(node: HasFilename, diffGraph: DiffGraph.Builder): Unit = {
    if (node.isInstanceOf[HasIsExternal] && systemIncludePaths.exists(p => node.filename.startsWith(p.toString))) {
      diffGraph.addNodeProperty(node.asInstanceOf[StoredNode], PropertyNames.IS_EXTERNAL, Boolean.box(true))
    }
  }

  private def createGlobalBlock(dstGraph: DiffGraph.Builder): NewBlock = {
    val includesFile = NewFile().name(filename).order(0)

    val namespaceBlock = NewNamespaceBlock()
      .name(globalName)
      .fullName(fullName)
      .filename(filename)
      .order(1)

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
      .order(1)
      .argumentIndex(1)
      .typeFullName("ANY")

    val methodReturn = NewMethodReturn()
      .code("RET")
      .evaluationStrategy(EvaluationStrategies.BY_VALUE)
      .typeFullName("ANY")
      .order(2)

    val ast = Ast(includesFile).withChild(
      Ast(namespaceBlock)
        .withChild(Ast(fakeGlobalIncludesMethod).withChild(Ast(blockNode)).withChild(Ast(methodReturn)))
    )

    Ast.storeInDiffGraph(ast, dstGraph)
    blockNode
  }

  private def createMissingAstEdges(dstGraph: DiffGraph.Builder): Unit = {
    val globalBlock = createGlobalBlock(dstGraph)
    Traversal(cpg.graph.nodes()).whereNot(_.inE(EdgeTypes.AST)).foreach {
      case srcNode: HasFilename if FileDefaults.isHeaderFile(srcNode.filename) =>
        dstGraph.addEdgeToOriginal(globalBlock, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
        setExternal(srcNode, dstGraph)
      case srcNode: Local =>
        dstGraph.addEdgeToOriginal(globalBlock, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
      case _ =>
    }
  }

  private def typeNeedsTypeDeclStub(t: Type): Boolean =
    !typeDeclFullNames.contains(t.typeDeclFullName)

  private def createMissingTypeDecls(dstGraph: DiffGraph.Builder): Unit = {
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

  override def run(): Iterator[DiffGraph] = {
    if (!Global.shouldBeCleared()) {
      Iterator.empty
    } else {
      val dstGraph = DiffGraph.newBuilder

      createMissingAstEdges(dstGraph)
      createMissingTypeDecls(dstGraph)

      Iterator(dstGraph.build())
    }
  }

}
