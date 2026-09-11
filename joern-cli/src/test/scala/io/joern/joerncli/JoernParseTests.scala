package io.joern.joerncli

import io.joern.dataflowengineoss.layers.dataflows.OssDataFlow
import io.joern.joerncli.JoernParse.ParserConfig
import io.joern.x2cpg.layers.Base
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class JoernParseTests extends AnyWordSpec with Matchers {

  "joern-parse --overlaysonly" should {
    // Regression test for https://github.com/joernio/joern/issues/6283: no frontend runs in
    // --overlaysonly mode, so the post-processing generator must be resolved from the CPG itself.
    "apply default overlays to an existing CPG without running a frontend" in {
      FileUtil.usingTemporaryDirectory("joern-parse-test") { tmpDir =>
        val cpgPath = tmpDir.resolve("cpg.bin")
        val cpg     = Cpg.withStorage(cpgPath)
        new MetaDataPass(cpg, Languages.PHP, tmpDir.toString).createAndApply()
        cpg.close()

        val config = ParserConfig(
          inputPath = cpgPath.toString,
          outputCpgFile = cpgPath.toString,
          language = "php",
          enhanceOnly = true
        )

        JoernParse.run(config) match {
          case Failure(exception) => fail("joern-parse --overlaysonly failed", exception)
          case Success(_) =>
            val enhancedCpg = CpgBasedTool.loadFromFile(cpgPath.toString)
            try {
              enhancedCpg.metaData.overlays.l should contain.allOf(Base.overlayName, OssDataFlow.overlayName)
            } finally {
              enhancedCpg.close()
            }
        }
      }
    }
  }
}
