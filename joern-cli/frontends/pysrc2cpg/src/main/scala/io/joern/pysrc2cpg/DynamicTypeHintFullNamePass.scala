package io.joern.pysrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.CpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

/** The type hints we pick up via the parser are not full names. This pass fixes that by retrieving the import for each
  * dynamic type hint and adjusting the dynamic type hint full name field accordingly.
  */
class DynamicTypeHintFullNamePass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    val fileToImports = cpg.imports.l
      .flatMap { imp =>
        imp.call.file.l.map { f => f.name -> imp }
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))

    for {
      methodReturn   <- cpg.methodReturn.filter(x => x.dynamicTypeHintFullName.nonEmpty)
      typeHint       <- methodReturn.dynamicTypeHintFullName
      file           <- methodReturn.file
      imports        <- fileToImports.get(file.name)
      importedEntity <- imports.importedAsExact(typeHint).importedEntity
    } {
      diffGraph.setNodeProperty(methodReturn, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, importedEntity)
    }
  }
}
