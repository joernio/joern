package io.joern.c2cpg.io

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.Cpg14DumpOptions
import io.joern.dataflowengineoss.layers.dataflows.DumpCpg14
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.Files

class DumpCpg14Tests extends DataFlowCodeToCpgSuite {

  "DumpCpg14" should {
    val cpg = code("""
      |int foo() {}
      |int bar() {}
      |""".stripMargin)

    "create two dot files for a CPG containing two methods" in {
      FileUtil.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts         = Cpg14DumpOptions(tmpDir.toString)
        val layerContext = new LayerCreatorContext(cpg)
        new DumpCpg14(opts).run(layerContext)
        Files.exists((tmpDir / "0-cpg.dot")) shouldBe true
        Files.exists((tmpDir / "1-cpg.dot")) shouldBe true
        Files.size((tmpDir / "0-cpg.dot")) should not be 0
        Files.size((tmpDir / "1-cpg.dot")) should not be 0
      }
    }

  }

}
