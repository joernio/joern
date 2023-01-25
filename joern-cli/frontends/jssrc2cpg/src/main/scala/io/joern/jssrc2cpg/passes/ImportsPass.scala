package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, NewImport}
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

/** This pass creates `IMPORT` nodes by looking for calls to `require`. `IMPORT` nodes are linked to existing dependency
  * nodes, or, if no suitable dependency node exists, a dependency node is created.
  */
class ImportsPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg
      .call("require")
      .inAssignment
      .codeNot("var .*")
      .foreach { assignment =>
        val call       = assignment.source.asInstanceOf[Call]
        val importedAs = assignment.target.code
        val importedEntity =
          RequirePass.stripQuotes(call.argument(1).code)
        val importNode = NewImport()
          .importedAs(importedAs)
          .importedEntity(importedEntity)
        diffGraph.addNode(importNode)
        diffGraph.addEdge(call, importNode, EdgeTypes.IS_CALL_FOR_IMPORT)
      }
  }
}
