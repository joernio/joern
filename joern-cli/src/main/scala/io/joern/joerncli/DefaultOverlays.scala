package io.joern.joerncli

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.joerncli.console.JoernWorkspaceLoader
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers._

object DefaultOverlays {

  val DEFAULT_CPG_IN_FILE           = "cpg.bin"
  val defaultMaxNumberOfDefinitions = 4000

  /** Load the CPG at `storeFilename` and add enhancements, turning the CPG into an SCPG.
    *
    * @param storeFilename
    *   the filename of the cpg
    */
  def create(storeFilename: String, maxNumberOfDefinitions: Int = defaultMaxNumberOfDefinitions): Cpg = {
    val cpg = CpgBasedTool.loadFromOdb(storeFilename)
    applyDefaultOverlays(cpg)
    val context                       = new LayerCreatorContext(cpg)
    val options                       = new OssDataFlowOptions(maxNumberOfDefinitions)
    implicit val semantics: Semantics = JoernWorkspaceLoader.defaultSemantics
    if (semantics.elements.isEmpty) {
      System.err.println("Warning: semantics are empty.")
    }
    new OssDataFlow(options).run(context)
    cpg
  }

}
