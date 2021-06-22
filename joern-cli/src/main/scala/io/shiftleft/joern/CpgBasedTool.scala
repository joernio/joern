package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.traversal._
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.language._

object CpgBasedTool {

  /**
    * Load code property graph from overflowDB
    * @param filename name of the file that stores the cpg
    * */
  def loadFromOdb(filename: String): Cpg = {
    val odbConfig = overflowdb.Config.withDefaults().withStorageLocation(filename)
    val config = CpgLoaderConfig().withOverflowConfig(odbConfig).doNotCreateIndexesOnLoad
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.loadFromOverflowDb(config)
  }

  def addDataFlowOverlayIfNonExistent(cpg: Cpg): Unit = {
    if (!cpg.metaData.overlays.exists(_ == OssDataFlow.overlayName)) {
      System.err.println("CPG does not have dataflow overlay. Calculating.")
      val opts = new OssDataFlowOptions()
      val context = new LayerCreatorContext(cpg)
      new OssDataFlow(opts).run(context)
    }
  }

}
