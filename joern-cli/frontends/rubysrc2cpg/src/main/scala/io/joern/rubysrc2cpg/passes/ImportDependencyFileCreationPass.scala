package io.joern.rubysrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Import, NewFile}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Checks for matching import and dependency nodes and creates a corresponding file node for each pair
  * @param cpg
  */
class ImportDependencyFileCreationPass(cpg: Cpg) extends ForkJoinParallelCpgPass[Import](cpg) {
  override def generateParts(): Array[Import] = cpg.imports.toArray
  override def runOnPart(diffGraph: DiffGraphBuilder, importNode: Import): Unit = {
    importNode.importedEntity.foreach { importedEntityName =>
      cpg.dependency.filter { x => importedEntityName == x.name }.foreach { x =>
        val gemFileNode = NewFile()
          .name(s"${x.name}.rb")

        diffGraph.addNode(gemFileNode)
      }
    }
  }
}
