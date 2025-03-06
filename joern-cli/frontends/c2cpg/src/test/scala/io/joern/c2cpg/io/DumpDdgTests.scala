package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.DdgDumpOptions
import io.joern.dataflowengineoss.layers.dataflows.DumpDdg
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.nio.file.Files

class DumpDdgTests extends DataFlowCodeToCpgSuite {

  "DumpDdg" should {
    val cpg = code("""
      |int foo() {}
      |int bar() {}
      |""".stripMargin)

    "create two dot files for a CPG containing two methods" in {
      FileUtil.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts         = DdgDumpOptions(tmpDir.toString)
        val layerContext = new LayerCreatorContext(cpg)
        new DumpDdg(opts).run(layerContext)
        Files.exists((tmpDir / "0-ddg.dot")) shouldBe true
        Files.exists((tmpDir / "1-ddg.dot")) shouldBe true
        Files.size((tmpDir / "0-ddg.dot")) should not be 0
        Files.size((tmpDir / "1-ddg.dot")) should not be 0
      }
    }

  }

}
