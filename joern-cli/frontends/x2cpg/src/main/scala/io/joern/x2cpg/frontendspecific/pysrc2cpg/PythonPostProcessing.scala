package io.joern.x2cpg.frontendspecific.pysrc2cpg

import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}

/** The passes that run on a CPG after the pysrc2cpg frontend. The layer name is stored in the CPG, so a CPG that was
  * already post-processed, e.g., by `joern-parse`, does not get the passes applied a second time.
  */
class PythonPostProcessing(typeRecoveryConfig: XTypeRecoveryConfig) extends LayerCreator {
  override val overlayName: String = "pysrc2cpg-postprocessing"
  override val description: String = "Post-processing passes of the pysrc2cpg frontend"

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    (List(
      new ImportsPass(cpg),
      new PythonImportResolverPass(cpg),
      new DynamicTypeHintFullNamePass(cpg),
      new PythonInheritanceNamePass(cpg)
    )
      ++ new PythonTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate()
      ++ List(
        new PythonTypeHintCallLinker(cpg),
        new NaiveCallLinker(cpg),
        // Some of passes above create new methods, so, we
        // need to run the ASTLinkerPass one more time
        new AstLinkerPass(cpg)
      )).foreach(_.createAndApply())
  }
}
