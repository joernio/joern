package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment

abstract class XImportsPass(cpg: Cpg) extends ForkJoinParallelCpgPass[(Call, Assignment)](cpg) {

  protected val importCallName: String

  override def generateParts(): Array[(Call, Assignment)] = cpg
    .call(importCallName)
    .flatMap(importCallToPart)
    .toArray

  protected def importCallToPart(x: Call): Iterator[(Call, Assignment)]

  override def runOnPart(diffGraph: DiffGraphBuilder, part: (Call, Assignment)): Unit = {
    val (call, assignment) = part
    val importedEntity     = importedEntityFromCall(call)
    val importedAs         = assignment.target.code
    createImportNodeAndLink(importedEntity, importedAs, Some(call), diffGraph)
  }

  protected def importedEntityFromCall(call: Call): String

}
