package io.joern.php2cpg.passes

import io.joern.php2cpg.astcreation.AstCreator
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

// TODO This is a hack for a customer issue. Either extend this to handle type full names properly,
//  or do it elsewhere.
class AnyTypePass(cpg: Cpg) extends ForkJoinParallelCpgPass[AstNode](cpg) {

  override def generateParts(): Array[AstNode] = {
    cpg.has(PropertyNames.TYPE_FULL_NAME, PropertyDefaults.TypeFullName).collectAll[AstNode].toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, node: AstNode): Unit = {
    diffGraph.setNodeProperty(node, PropertyNames.TYPE_FULL_NAME, AstCreator.TypeConstants.Any)
  }
}
