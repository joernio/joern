package io.joern.rubysrc2cpg.passes

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.joern.x2cpg.X2Cpg.stripQuotes
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

class ImportsPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Call](cpg) {

  private val importCallName: Seq[String] = Seq("require", "load", "require_relative", "require_all")

  override def generateParts(): Array[Call] = cpg.call.nameExact(importCallName*).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, call: Call): Unit = {
    val importedEntity = stripQuotes(call.argument.isLiteral.code.l match {
      case s :: _ => s
      case _      => ""
    })
    val importNode = createImportNodeAndLink(importedEntity, importedEntity, Some(call), diffGraph)
    if (call.name == "require_all") importNode.isWildcard(true)
  }
}
