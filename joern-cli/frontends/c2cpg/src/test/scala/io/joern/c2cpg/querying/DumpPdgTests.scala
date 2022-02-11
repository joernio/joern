package io.joern.c2cpg.querying

import better.files.File
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.layers.dataflows.{DumpPdg, PdgDumpOptions}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.nio.file.Files

class DumpPdgTests extends DataFlowCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {}
      |int bar() {}
      |""".stripMargin

  "DumpPdg" should {

    "create two dot files for a CPG containing two methods" in {

      File.usingTemporaryDirectory("dumppdg") { tmpDir =>
        val opts                  = PdgDumpOptions(tmpDir.path.toString)
        implicit val s: Semantics = semantics
        val layerContext          = new LayerCreatorContext(cpg)
        new DumpPdg(opts).run(layerContext)
        (tmpDir / "0-pdg.dot").exists shouldBe true
        (tmpDir / "1-pdg.dot").exists shouldBe true
        Files.size((tmpDir / "0-pdg.dot").path) should not be 0
        Files.size((tmpDir / "1-pdg.dot").path) should not be 0
      }
    }

  }

}
