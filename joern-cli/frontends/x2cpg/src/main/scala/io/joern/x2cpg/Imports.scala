package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{CallBase, NewCall, NewImport}
import overflowdb.BatchedUpdate.DiffGraphBuilder

object Imports {

  def createImportNodeAndLink(
    importedEntity: String,
    importedAs: String,
    call: Option[CallBase],
    diffGraph: DiffGraphBuilder
  ): NewImport = {
    createImportNodeAndAttachToCall("", importedEntity, importedAs, call, diffGraph)
  }

  def createImportNodeAndAttachToCall(
    code: String,
    importedEntity: String,
    importedAs: String,
    call: Option[CallBase],
    diffGraph: DiffGraphBuilder
  ): NewImport = {
    val impNode = NewImport()
      .code(code)
      .importedEntity(importedEntity)
      .importedAs(importedAs)
      .lineNumber(call.flatMap(_.lineNumber))
      .columnNumber(call.flatMap(_.lineNumber))
    call.foreach { c => diffGraph.addEdge(c, impNode, EdgeTypes.IS_CALL_FOR_IMPORT) }
    impNode
  }

}
