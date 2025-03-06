package io.joern.x2cpg.layers

import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class DumpCdgTests extends AnyWordSpec with Matchers {

  "DumpCdg" should {

    "create two dot files for a CPG containing two methods" in {
      val cpg = MockCpg()
        .withMetaData()
        .withMethod("foo")
        .withMethod("bar")
        .cpg

      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      new ControlFlow().run(context)
      FileUtil.usingTemporaryDirectory("dumpcdg") { tmpDir =>
        val opts = CdgDumpOptions(tmpDir.toString)
        new DumpCdg(opts).run(context)
        Files.exists(tmpDir / "0-cdg.dot") shouldBe true
        Files.exists(tmpDir / "1-cdg.dot") shouldBe true
        Files.size((tmpDir / "0-cdg.dot")) should not be 0
        Files.size((tmpDir / "1-cdg.dot")) should not be 0
      }
    }

  }

}
