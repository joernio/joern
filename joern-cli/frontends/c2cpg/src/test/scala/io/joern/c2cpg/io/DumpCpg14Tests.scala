package io.joern.c2cpg.io

import better.files.File
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.Cpg14DumpOptions
import io.joern.dataflowengineoss.layers.dataflows.DumpCpg14
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.nio.file.Files

class DumpCpg14Tests extends DataFlowCodeToCpgSuite {

  "DumpCpg14" should {
    val cpg = code("""
      |int foo() {}
      |int bar() {}
      |""".stripMargin)

    "create two dot files for a CPG containing two methods" in {
      File.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts         = Cpg14DumpOptions(tmpDir.path.toString)
        val layerContext = new LayerCreatorContext(cpg)
        new DumpCpg14(opts).run(layerContext)
        (tmpDir / "0-cpg.dot").exists shouldBe true
        (tmpDir / "1-cpg.dot").exists shouldBe true
        Files.size((tmpDir / "0-cpg.dot").path) should not be 0
        Files.size((tmpDir / "1-cpg.dot").path) should not be 0
      }
    }

  }

}
