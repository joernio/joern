package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{NewMetaData, NewNamespaceBlock}
import io.shiftleft.passes.{KeyPool, SimpleCpgPass}
import io.shiftleft.semanticcpg.language.types.structure.{FileTraversal, NamespaceTraversal}

/** A pass that creates a MetaData node, specifying that this is a CPG for language, and a NamespaceBlock for anything
  * that cannot be assigned to any other namespace.
  */
class MetaDataPass(cpg: Cpg, language: String, keyPool: Option[KeyPool] = None)
    extends SimpleCpgPass(cpg, keyPool = keyPool) {
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    def addMetaDataNode(diffGraph: DiffGraphBuilder): Unit = {
      val metaNode = NewMetaData().language(language).version("0.1")
      diffGraph.addNode(metaNode)
    }

    def addAnyNamespaceBlock(diffGraph: DiffGraphBuilder): Unit = {
      val node = NewNamespaceBlock()
        .name(NamespaceTraversal.globalNamespaceName)
        .fullName(MetaDataPass.getGlobalNamespaceBlockFullName(None))
        .filename(FileTraversal.UNKNOWN)
        .order(1)
      diffGraph.addNode(node)
    }

    addMetaDataNode(diffGraph)
    addAnyNamespaceBlock(diffGraph)
  }
}

object MetaDataPass {

  def getGlobalNamespaceBlockFullName(fileNameOption: Option[String]): String = {
    fileNameOption match {
      case Some(fileName) =>
        s"$fileName:${NamespaceTraversal.globalNamespaceName}"
      case None =>
        NamespaceTraversal.globalNamespaceName
    }
  }

}
