package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import io.shiftleft.semanticcpg.language._

class MinifiedFileDetectionTest extends AnyWordSpec with Matchers {

  "Detecting minified files" should {
    "skip minified files by name correctly" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "a.min.js").createFile()
        (sourceDir / "a.min.23472420.js").createFile()
        (sourceDir / "b-min.js").createFile()
        (sourceDir / "b-min.23472420.js").createFile()
        (sourceDir / "c.bundle.js").createFile()
        val cpg          = newEmptyCpg()
        val config       = Config(inputPath = sourceDir.toString, outputPath = sourceDir.toString)
        val astgenResult = AstGenRunner.execute(config, sourceDir)
        new AstCreationPass(cpg, astgenResult, config).createAndApply()
        cpg.file.name.l shouldBe empty
      }
    }

    "skip minified files by content correctly" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        val minFile = (sourceDir / "something.js").createFile()
        minFile.write(s"console.log('${"x" * 10000}');")
        val cpg          = newEmptyCpg()
        val config       = Config(inputPath = sourceDir.toString, outputPath = sourceDir.toString)
        val astgenResult = AstGenRunner.execute(config, sourceDir)
        new AstCreationPass(cpg, astgenResult, config).createAndApply()
        cpg.file.name.l shouldBe empty
      }
    }

  }

}
