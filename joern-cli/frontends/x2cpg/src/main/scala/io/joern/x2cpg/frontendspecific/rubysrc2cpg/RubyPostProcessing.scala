package io.joern.x2cpg.frontendspecific.rubysrc2cpg

import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}

/** The passes that run on a CPG after the rubysrc2cpg frontend. The layer name is stored in the CPG, so a CPG that was
  * already post-processed, e.g., by `joern-parse`, does not get the passes applied a second time.
  */
class RubyPostProcessing(typeRecoveryConfig: XTypeRecoveryConfig = XTypeRecoveryConfig(iterations = 4))
    extends LayerCreator {
  override val overlayName: String = "rubysrc2cpg-postprocessing"
  override val description: String = "Post-processing passes of the rubysrc2cpg frontend"

  override def create(context: LayerCreatorContext): Unit = {
    val cpg                 = context.cpg
    val implicitRequirePass = if (cpg.dependency.name.contains("zeitwerk")) ImplicitRequirePass(cpg) :: Nil else Nil
    (implicitRequirePass ++ List(ImportsPass(cpg), RubyImportResolverPass(cpg)) ++
      new RubyTypeRecoveryPassGenerator(cpg, config = typeRecoveryConfig)
        .generate() ++ List(new RubyTypeHintCallLinker(cpg), new NaiveCallLinker(cpg), new AstLinkerPass(cpg)))
      .foreach(_.createAndApply())
  }
}
