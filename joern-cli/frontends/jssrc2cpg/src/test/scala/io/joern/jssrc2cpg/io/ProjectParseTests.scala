package io.joern.jssrc2cpg.io

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

import java.nio.file.{Files, Path}

class ProjectParseTests extends JsSrc2CpgSuite with BeforeAndAfterAll {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  private val projectWithSubfolders: Path = {
    val dir = Files.createTempDirectory("jssrc2cpgTestsSubfolders")
    List("sub/c.js", "sub/d.js", "a.js", "b.js").foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, s"""console.log("${file.toAbsolutePath.toString}");""")
    }
    dir
  }

  private val projectWithBrokenFile: Path = {
    val dir      = Files.createTempDirectory("jssrc2cpgTestsBroken")
    val goodFile = dir / "good.js"
    goodFile.createWithParentsIfNotExists(createParents = true)
    Files.writeString(goodFile, s"""console.log("good");""")
    val brokenFile = dir / "broken.js"
    brokenFile.createWithParentsIfNotExists(createParents = true)
    Files.writeString(brokenFile, s"""console.log("broken""")
    dir
  }

  private val projectWithUtf8: Path = {
    val dir  = Files.createTempDirectory("jssrc2cpgTestsUtf8")
    val file = dir / "utf8.js"
    file.createWithParentsIfNotExists(createParents = true)
    val content = """
                   |// 😼
                   |logger.error()
                   |""".stripMargin
    Files.writeString(file, content)
    dir
  }

  private val projectWithStrangeFilenames: Path = {
    val dir = Files.createTempDirectory("jssrc2cpgTestsFilenames")
    List("good_%component-name%_.js", "good_%component-name%_Foo.js").foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, s"""console.log("${file.toAbsolutePath.toString}");""")
    }
    List("broken_%component-name%_.js", "broken_%component-name%_Foo.js").foreach { testFile =>
      val file = dir / testFile
      file.createWithParentsIfNotExists(createParents = true)
      Files.writeString(file, s"""const x = new <%ComponentName%>Foo();""")
    }
    dir
  }

  override def afterAll(): Unit = {
    FileUtil.delete(projectWithSubfolders, swallowIoExceptions = true)
    FileUtil.delete(projectWithBrokenFile, swallowIoExceptions = true)
    FileUtil.delete(projectWithUtf8, swallowIoExceptions = true)
    FileUtil.delete(projectWithStrangeFilenames, swallowIoExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: Path)(f: Cpg => Unit): Unit = {
      FileUtil.usingTemporaryDirectory("jssrc2cpgTests") { tmpDir =>
        val cpg          = newEmptyCpg()
        val config       = Config(tsTypes = false).withInputPath(projectDir.toString).withOutputPath(tmpDir.toString)
        val astGenResult = new AstGenRunner(config).execute(tmpDir)
        new AstCreationPass(cpg, astGenResult, config).createAndApply()
        f(cpg)
      }
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture(projectWithSubfolders) { cpg =>
      cpg.file.name.l should contain allElementsOf List(
        "a.js",
        "b.js",
        s"sub${java.io.File.separator}c.js",
        s"sub${java.io.File.separator}d.js"
      )
    }

    "recover from broken input file" in ProjectParseTestsFixture(projectWithBrokenFile) { cpg =>
      cpg.file.name.l should (contain("good.js") and not contain "broken.js")
    }

    "handle utf8 correctly" in ProjectParseTestsFixture(projectWithUtf8) { cpg =>
      cpg.fieldAccess.argument(2).code.l shouldBe List("error")
    }

    "handle strange filenames correctly" in ProjectParseTestsFixture(projectWithStrangeFilenames) { cpg =>
      cpg.file.name.l shouldBe List("good_%component-name%_.js", "good_%component-name%_Foo.js")
    }

  }

}
