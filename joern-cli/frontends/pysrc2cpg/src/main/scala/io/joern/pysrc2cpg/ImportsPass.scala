package io.joern.pysrc2cpg

import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._

class ImportsPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val importsAndAssignments = cpg
      .call("import")
      .flatMap { x =>
        x.inAssignment.map(y => (x, y))
      }

    importsAndAssignments.foreach { case (call, assignment) =>
      val importedEntity = importedEntityFromCall(call)
      val importedAs     = assignment.target.code
      createImportNodeAndLink(importedEntity, importedAs, Some(call), diffGraph)
    }
  }

  def importedEntityFromCall(call: Call): String = {
    call.argument.code.l match {
      case List("", what)       => what
      case List(where, what)    => s"$where.$what"
      case List("", what, _)    => what
      case List(where, what, _) => s"$where.$what"
      case _                    => ""
    }
  }

}
