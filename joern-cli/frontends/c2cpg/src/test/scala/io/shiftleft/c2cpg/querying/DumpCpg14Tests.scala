package io.shiftleft.c2cpg.querying

import better.files.File
import io.shiftleft.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.dataflowengineoss.layers.dataflows.{Cpg14DumpOptions, DumpCpg14}
import io.shiftleft.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DumpCpg14Tests extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {}
      |int bar() {}
      |""".stripMargin

  "DumpCpg14" should {

    "create two dot files for a CPG containing two methods" in {

      File.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts = Cpg14DumpOptions(tmpDir.path.toString)
        implicit val s: Semantics = semantics
        val layerContext = new LayerCreatorContext(cpg)
        new DumpCpg14(opts).run(layerContext)
        (tmpDir / "0-cpg.dot").exists shouldBe true
        (tmpDir / "1-cpg.dot").exists shouldBe true
        (tmpDir / "0-cpg.dot").size should not be 0
        (tmpDir / "1-cpg.dot").size should not be 0
      }
    }

  }

}
