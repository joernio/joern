package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.v2.EdgeTypes
import io.shiftleft.codepropertygraph.generated.v2.nodes.{CallBase, NewImport}
import flatgraph.DiffGraphBuilder

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
    call.foreach { c => diffGraph.addEdge(c, importNode, EdgeTypes.IS_CALL_FOR_IMPORT) }
    importNode
  }

}
