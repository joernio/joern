package io.joern.jssrc2cpg.passes

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

/** This pass creates `IMPORT` nodes by looking for calls to `require`. `IMPORT` nodes are linked to existing dependency
  * nodes, or, if no suitable dependency node exists, a dependency node is created.
  *
  * TODO with this, we can have multiple IMPORT nodes that point to the same call: one created during AST creation, and
  * one using this pass.
  *
  * TODO Dependency node creation is still missing.
  */
class ImportsPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg
      .call("require")
      .flatMap { x => x.inAssignment.codeNot("var .*").map(y => (x, y)) }
      .foreach { case (call, assignment) =>
        val importedAs     = assignment.target.code
        val importedEntity = X2Cpg.stripQuotes(call.argument(1).code)
        createImportNodeAndLink(importedEntity, importedAs, Option(call), diffGraph)
      }
  }

}
