package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.DumpPdg
import io.joern.dataflowengineoss.layers.dataflows.PdgDumpOptions
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.nio.file.Files

class DumpPdgTests extends DataFlowCodeToCpgSuite {

  "DumpPdg" should {
    val cpg = code("""
      |int foo() {}
      |int bar() {}
      |""".stripMargin)

    "create two dot files for a CPG containing two methods" in {
      FileUtil.usingTemporaryDirectory("dumppdg") { tmpDir =>
        val opts         = PdgDumpOptions(tmpDir.toString)
        val layerContext = new LayerCreatorContext(cpg)
        new DumpPdg(opts).run(layerContext)
        Files.exists((tmpDir / "0-pdg.dot")) shouldBe true
        Files.exists((tmpDir / "1-pdg.dot")) shouldBe true
        Files.size((tmpDir / "0-pdg.dot")) should not be 0
        Files.size((tmpDir / "1-pdg.dot")) should not be 0
      }
    }

  }

}
