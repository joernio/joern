package io.joern.x2cpg.layers

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.testing.MockCpg
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class DumpCfgTests extends AnyWordSpec with Matchers {

  "DumpCfg" should {

    "create two dot files for a CPG containing two methods" in {
      val cpg = MockCpg()
        .withMetaData()
        .withMethod("foo")
        .withMethod("bar")
        .cpg

      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      new ControlFlow().run(context)
      FileUtil.usingTemporaryDirectory("dumpcfg") { tmpDir =>
        val opts = CfgDumpOptions(tmpDir.toString)
        new DumpCfg(opts).run(context)
        Files.exists((tmpDir / "0-cfg.dot")) shouldBe true
        Files.exists((tmpDir / "1-cfg.dot")) shouldBe true
        Files.size((tmpDir / "0-cfg.dot")) should not be 0
        Files.size((tmpDir / "1-cfg.dot")) should not be 0
      }
    }

  }

}
