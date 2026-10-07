package io.joern.console.cpgcreation

import io.joern.console.FrontendConfig
import io.joern.x2cpg.frontendspecific.javasrc2cpg
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.Overlays
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class PostProcessingLayerTests extends AnyWordSpec with Matchers {

  private val root          = Path.of("")
  private val typeRecovery  = FrontendConfig().withArgs(Seq(s"--${javasrc2cpg.ParameterNames.EnableTypeRecovery}"))
  private val generatorsFor = List(
    (Languages.JAVASRC, JavaSrcCpgGenerator(typeRecovery, root), "javasrc2cpg-postprocessing"),
    (Languages.JSSRC, JsSrcCpgGenerator(FrontendConfig(), root), "jssrc2cpg-postprocessing"),
    (Languages.PHP, PhpCpgGenerator(FrontendConfig(), root), "php2cpg-postprocessing"),
    (Languages.PYTHONSRC, PythonSrcCpgGenerator(FrontendConfig(), root), "pysrc2cpg-postprocessing"),
    (Languages.RUBYSRC, RubyCpgGenerator(FrontendConfig(), root), "rubysrc2cpg-postprocessing"),
    (Languages.SWIFTSRC, SwiftSrcCpgGenerator(FrontendConfig(), root), "swiftsrc2cpg-postprocessing")
  )

  "applyPostProcessingPasses" should {
    generatorsFor.foreach { case (language, generator, layerName) =>
      s"apply the $language passes only once" in {
        val cpg = Cpg.empty
        new MetaDataPass(cpg, language, root.toString).createAndApply()

        generator.applyPostProcessingPasses(cpg)
        generator.applyPostProcessingPasses(cpg)

        Overlays.appliedOverlays(cpg) shouldBe List(layerName)
        cpg.close()
      }
    }

    "leave the CPG alone for a frontend without a post-processing layer" in {
      val cpg = Cpg.empty
      new MetaDataPass(cpg, Languages.C, root.toString).createAndApply()

      CCpgGenerator(FrontendConfig(), root).applyPostProcessingPasses(cpg)

      Overlays.appliedOverlays(cpg) shouldBe empty
      cpg.close()
    }
  }

}
