package io.joern.x2cpg.frontendspecific.php2cpg

import io.joern.x2cpg.passes.frontend.{XTypeRecoveryConfig, XTypeStubsParserConfig}
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}

/** The passes that run on a CPG after the php2cpg frontend. The layer name is stored in the CPG, so a CPG that was
  * already post-processed, e.g., by `joern-parse`, does not get the passes applied a second time.
  */
class PhpPostProcessing(
  typeRecoveryConfig: XTypeRecoveryConfig = XTypeRecoveryConfig(iterations = 3),
  setKnownTypesConfig: XTypeStubsParserConfig = XTypeStubsParserConfig()
) extends LayerCreator {
  override val overlayName: String = "php2cpg-postprocessing"
  override val description: String = "Post-processing passes of the php2cpg frontend"

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    (List(
      new ComposerAutoloadPass(cpg),
      new PhpTypeStubsParserPass(cpg, setKnownTypesConfig),
      new PhpImportResolverPass(cpg)
    ) ++ new PhpTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate() :+ PhpTypeHintCallLinker(cpg))
      .foreach(_.createAndApply())
  }
}
