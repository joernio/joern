package io.joern.c2cpg.passes

import io.joern.c2cpg.C2Cpg.Config
import io.joern.c2cpg.datastructures.Global
import io.joern.c2cpg.parser.FileDefaults
import io.joern.c2cpg.utils.IncludeAutoDiscovery
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{
  HasFilename,
  HasIsExternal,
  Local,
  NewBlock,
  NewFile,
  NewMethod,
  NewMethodReturn,
  NewNamespaceBlock,
  StoredNode
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, EvaluationStrategies, NodeTypes, PropertyNames}
import io.shiftleft.passes.{CpgPass, DiffGraph}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import io.shiftleft.x2cpg.Ast
import overflowdb.traversal.Traversal

import scala.tools.nsc.io.File

class HeaderContentLinkerPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  private val systemIncludePaths = IncludeAutoDiscovery.discoverIncludePaths(config)

  private def setExternal(node: HasFilename, diffGraph: DiffGraph.Builder): Unit = {
    if (node.isInstanceOf[HasIsExternal] && systemIncludePaths.exists(p => node.filename.startsWith(p.toString))) {
      diffGraph.addNodeProperty(node.asInstanceOf[StoredNode], PropertyNames.IS_EXTERNAL, Boolean.box(true))
    }
  }

  override def run(): Iterator[DiffGraph] = {
    if (!Global.shouldBeCleared()) {
      Iterator.empty
    } else {
      val dstGraph = DiffGraph.newBuilder
      val absolutePath = File(config.inputPaths.head).toAbsolute.normalize.toString
      val filename = s"$absolutePath:<includes>"
      val globalName = NamespaceTraversal.globalNamespaceName

      val includesFile = NewFile().name(filename).order(0)

      val fullName = MetaDataPass.getGlobalNamespaceBlockFullName(Some(filename))
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
        Ast(namespaceBlock).withChild(
          Ast(fakeGlobalIncludesMethod).withChild(Ast(blockNode)).withChild(Ast(methodReturn))))

      Ast.storeInDiffGraph(ast, dstGraph)

      Traversal(cpg.graph.nodes()).whereNot(_.inE(EdgeTypes.AST)).foreach {
        case srcNode: HasFilename if FileDefaults.isHeaderFile(srcNode.filename) =>
          dstGraph.addEdgeToOriginal(blockNode, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
          setExternal(srcNode, dstGraph)
        case srcNode: Local =>
          dstGraph.addEdgeToOriginal(blockNode, srcNode.asInstanceOf[StoredNode], EdgeTypes.AST)
        case _ =>
      }

      Iterator(dstGraph.build())
    }
  }

}
