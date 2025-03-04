package io.joern.x2cpg.layers

import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class DumpAstTests extends AnyWordSpec with Matchers {

  "DumpAst" should {

    "create two dot files for a CPG containing two methods" in {
      val cpg = MockCpg()
        .withMetaData()
        .withMethod("foo")
        .withMethod("bar")
        .cpg

      val context = new LayerCreatorContext(cpg)
      new Base().run(context)
      FileUtil.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts = AstDumpOptions(tmpDir.toString)
        new DumpAst(opts).run(context)
        Files.exists(tmpDir / "0-ast.dot") shouldBe true
        Files.exists(tmpDir / "1-ast.dot") shouldBe true
        Files.size(tmpDir / "0-ast.dot") should not be 0
        Files.size(tmpDir / "1-ast.dot") should not be 0
      }
    }

  }

}
