package io.joern.x2cpg.frontendspecific.swiftsrc2cpg

import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}

/** The passes that run on a CPG after the swiftsrc2cpg frontend. The layer name is stored in the CPG, so a CPG that was
  * already post-processed, e.g., by `joern-parse`, does not get the passes applied a second time.
  */
class SwiftPostProcessing(typeRecoveryConfig: XTypeRecoveryConfig) extends LayerCreator {
  override val overlayName: String = "swiftsrc2cpg-postprocessing"
  override val description: String = "Post-processing passes of the swiftsrc2cpg frontend"

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    (List(new SwiftInheritanceNamePass(cpg), new ConstClosurePass(cpg)) ++
      new SwiftTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() ++ List(
        new SwiftTypeHintCallLinker(cpg),
        new NaiveCallLinker(cpg)
      )).foreach(_.createAndApply())
  }
}
