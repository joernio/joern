package io.joern.x2cpg.passes.frontend

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
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
    val importNode         = createImportNodeAndLink(importedEntity, importedAs, Some(call), diffGraph)

    importNode
      .code(importCode(call))
      .order(call.order)
      .lineNumber(call.lineNumber)
      .columnNumber(call.columnNumber)

    call.inAst.collectAll[nodes.Method].headOption.foreach { m =>
      diffGraph.addEdge(m, importNode, EdgeTypes.AST)
    }
  }

  /** Hook for frontends to customize how the import code string is rendered (defaults to lowering in call.code). */
  protected def importCode(call: Call): String = call.code

  protected def importedEntityFromCall(call: Call): String

}
