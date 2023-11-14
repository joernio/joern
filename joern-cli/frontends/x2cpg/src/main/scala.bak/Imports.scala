package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.v2.EdgeKinds
import io.shiftleft.codepropertygraph.generated.v2.nodes.{CallBase, NewImport}
import io.joern.odb2.DiffGraphBuilder

object Imports {

  def createImportNodeAndLink(
    importedEntity: String,
    importedAs: String,
    call: Option[CallBase],
    diffGraph: DiffGraphBuilder
  ): NewImport = {
    val importNode = NewImport()
      .importedAs(importedAs)
      .importedEntity(importedEntity)
    diffGraph.addNode(importNode)
    call.foreach { c => diffGraph.addEdge(c, importNode, EdgeKinds.IS_CALL_FOR_IMPORT) }
    importNode
  }

}
