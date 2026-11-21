package io.joern.dataflowengineoss.layers.dataflows

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefPass
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics, Semantics}
import io.joern.x2cpg.Defines as X2CpgDefines
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

object OssDataFlow {
  val overlayName: String = "dataflowOss"
  val description: String = "Layer to support the OSS lightweight data flow tracker"

  def defaultOpts = new OssDataFlowOptions()
}

/** @param byValueArguments
  *   whether a call does not define an argument its callee receives by value; off unless the system property
  *   `joern.byValueArguments` is `true`
  */
class OssDataFlowOptions(
  var maxNumberOfDefinitions: Int = 4000,
  var semantics: Semantics = DefaultSemantics(),
  var byValueArguments: Boolean = X2CpgDefines.byValueArgumentsEnabled
) extends LayerCreatorOptions {}

class OssDataFlow(opts: OssDataFlowOptions)(implicit val semantics: Semantics = opts.semantics) extends LayerCreator {

  override val overlayName: String = OssDataFlow.overlayName
  override val description: String = OssDataFlow.description

  override def create(context: LayerCreatorContext): Unit = {
    ReachingDefPass(context.cpg, opts.maxNumberOfDefinitions, opts.byValueArguments).createAndApply()
  }
}
