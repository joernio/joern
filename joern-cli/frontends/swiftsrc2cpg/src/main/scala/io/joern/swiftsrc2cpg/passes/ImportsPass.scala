package io.joern.swiftsrc2cpg.passes

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.frontend.XImportsPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment

/** This pass creates `IMPORT` nodes by looking for calls to `require`. `IMPORT` nodes are linked to existing dependency
  * nodes, or, if no suitable dependency node exists, a dependency node is created.
  *
  * TODO with this, we can have multiple IMPORT nodes that point to the same call: one created during AST creation, and
  * one using this pass.
  *
  * TODO Dependency node creation is still missing.
  */
class ImportsPass(cpg: Cpg) extends XImportsPass(cpg) {

  override protected val importCallName: String = "import"

  override protected def importCallToPart(x: Call): Iterator[(Call, Assignment)] =
    x.inAssignment.codeNot("var .*").map(y => (x, y))

  override protected def importedEntityFromCall(call: Call): String = X2Cpg.stripQuotes(call.argument(1).code)

}
