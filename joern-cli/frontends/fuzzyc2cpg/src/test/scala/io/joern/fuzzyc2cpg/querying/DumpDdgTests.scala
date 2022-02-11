package io.joern.fuzzyc2cpg.querying

import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{DdgDumpOptions, DumpDdg}
import io.joern.fuzzyc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DumpDdgTests extends DataFlowCodeToCpgSuite {

  override val code =
    """
      |int foo() {}
      |int bar() {}
      |""".stripMargin

  "DumpDdg" should {

    "create two dot files for a CPG containing two methods" in {

      File.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts         = DdgDumpOptions(tmpDir.path.toString)
        implicit val s   = semantics
        val layerContext = new LayerCreatorContext(cpg)
        new DumpDdg(opts).run(layerContext)
        (tmpDir / "0-ddg.dot").exists shouldBe true
        (tmpDir / "1-ddg.dot").exists shouldBe true
        (tmpDir / "0-ddg.dot").size should not be 0
        (tmpDir / "1-ddg.dot").size should not be 0
      }
    }

  }

}
