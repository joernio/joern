package io.joern.rubysrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.joern.x2cpg.X2Cpg.stripQuotes

class ImportsPass(cpg: Cpg) extends ConcurrentWriterCpgPass[Call](cpg) {
  protected val importCallName: String = "require"
  override def generateParts(): Array[Call] = cpg
    .call(importCallName)
    .toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    val importedEntity = stripQuotes(call.argument.isLiteral.code.l match {
      case s :: _ => s
      case _      => ""
    })
    createImportNodeAndLink(importedEntity, importedEntity, Some(call), diffGraph)
  }
}
