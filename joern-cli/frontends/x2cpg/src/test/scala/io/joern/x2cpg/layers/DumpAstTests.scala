package io.joern.x2cpg.layers

import better.files.File
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
      File.usingTemporaryDirectory("dumpast") { tmpDir =>
        val opts = AstDumpOptions(tmpDir.path.toString)
        new DumpAst(opts).run(context)
        (tmpDir / "0-ast.dot").exists shouldBe true
        (tmpDir / "1-ast.dot").exists shouldBe true
        Files.size((tmpDir / "0-ast.dot").path) should not be 0
        Files.size((tmpDir / "1-ast.dot").path) should not be 0
      }
    }

  }

}
