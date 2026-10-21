package io.joern.x2cpg.frontendspecific.javasrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}

/** The type recovery passes that run on a CPG after the javasrc2cpg frontend. The layer name is stored in the CPG, so a
  * CPG that was already post-processed, e.g., by `joern-parse`, does not get the passes applied a second time.
  */
class JavaPostProcessing(typeRecoveryConfig: XTypeRecoveryConfig) extends LayerCreator {
  override val overlayName: String = "javasrc2cpg-postprocessing"
  override val description: String = "Post-processing passes of the javasrc2cpg frontend"

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    (new JavaTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() :+ new JavaTypeHintCallLinker(cpg))
      .foreach(_.createAndApply())
  }
}
