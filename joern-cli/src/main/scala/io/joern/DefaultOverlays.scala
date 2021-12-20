package io.joern

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers._
import io.shiftleft.codepropertygraph.Cpg

object DefaultOverlays {

  val DEFAULT_CPG_IN_FILE = "cpg.bin"

  /** Load the CPG at `storeFilename` and add enhancements,
    * turning the CPG into an SCPG.
    * @param storeFilename the filename of the cpg
    */
  def create(storeFilename: String): Cpg = {
    val cpg = CpgBasedTool.loadFromOdb(storeFilename)
    val context = new LayerCreatorContext(cpg)
    new Base().run(context)
    new TypeRelations().run(context)
    new ControlFlow().run(context)
    new CallGraph().run(context)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    cpg
  }

}
