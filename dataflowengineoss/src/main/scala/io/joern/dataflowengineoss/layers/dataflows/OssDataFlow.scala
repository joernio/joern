package io.joern.dataflowengineoss.layers.dataflows

import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

object OssDataFlow {
  val overlayName: String = "dataflowOss"
  val description: String = "Layer to support the OSS lightweight data flow tracker"

  def defaultOpts = new OssDataFlowOptions()
}

class OssDataFlowOptions(var maxNumberOfDefinitions: Int = 4000) extends LayerCreatorOptions {}

class OssDataFlow(opts: OssDataFlowOptions) extends LayerCreator {

  override val overlayName: String = OssDataFlow.overlayName
  override val description: String = OssDataFlow.description

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg                 = context.cpg
    val enhancementExecList = Iterator(new ReachingDefPass(cpg, opts.maxNumberOfDefinitions))
    enhancementExecList.zipWithIndex.foreach { case (pass, index) =>
      runPass(pass, context, storeUndoInfo, index)
    }
  }
}
