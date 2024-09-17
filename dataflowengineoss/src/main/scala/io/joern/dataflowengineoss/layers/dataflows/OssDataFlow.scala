package io.joern.dataflowengineoss.layers.dataflows

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics, Semantics}
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

object OssDataFlow {
  val overlayName: String = "dataflowOss"
  val description: String = "Layer to support the OSS lightweight data flow tracker"

  def defaultOpts = new OssDataFlowOptions()
}

class OssDataFlowOptions(
  var maxNumberOfDefinitions: Int = 4000,
  var extraFlows: List[FlowSemantic] = List.empty[FlowSemantic]
) extends LayerCreatorOptions {}

class OssDataFlow(opts: OssDataFlowOptions)(implicit
  s: Semantics = FullNameSemantics.fromList(DefaultSemantics().elements ++ opts.extraFlows)
) extends LayerCreator {

  override val overlayName: String = OssDataFlow.overlayName
  override val description: String = OssDataFlow.description

  override def create(context: LayerCreatorContext): Unit = {
    ReachingDefPass(context.cpg, opts.maxNumberOfDefinitions).createAndApply()
  }
}
