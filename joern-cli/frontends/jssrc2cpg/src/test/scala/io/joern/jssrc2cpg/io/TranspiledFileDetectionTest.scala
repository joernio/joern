package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TranspiledFileDetectionTest extends AnyWordSpec with Matchers {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  "Detecting transpiled files" should {
    "skip transpiled files correctly (with source map comment)" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.ts").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js")
          .createFile()
          .writeText("""
            |console.log('Hello World!');
            |//sourceMappingURL=foo
            |""".stripMargin)
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.ts")
      }
    }

    "skip transpiled files correctly (with source map file)" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.ts").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js.map").createFile()
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.ts")
      }
    }

    "skip transpiled files correctly (with other file types)" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.vue").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.ejs").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js.map").createFile()
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.ejs", "index.vue")
      }
    }

    "not skip transpiled files when there is no source map file or comment" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.ts").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js").createFile().writeText("console.log('Hello World!');")
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.js", "index.ts")
      }
    }

    "not skip transpiled files when there is no sibling file with same name but a source map file only" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.js").createFile().writeText("console.log('Hello World!');")
        (sourceDir / "index.js.map").createFile()
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.js")
      }
    }

    "not skip transpiled files when there is no sibling file with same name but a source map comment only" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { sourceDir =>
        (sourceDir / "index.js")
          .createFile()
          .writeText("""
              |console.log('Hello World!');
              |//sourceMappingURL=foo
              |""".stripMargin)
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(sourceDir.toString).withOutputPath(sourceDir.toString)
        val astGenResult = new AstGenRunner(config).execute(sourceDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        cpg.file.name.l shouldBe List("index.js")
      }
    }
  }

}
