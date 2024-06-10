package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll

class ProjectParseTests extends JsSrc2CpgSuite with BeforeAndAfterAll {

  private implicit val schemaValidationMode: ValidationMode = ValidationMode.Enabled

  private val projectWithSubfolders: File = {
    val dir = File.newTemporaryDirectory("jssrc2cpgTestsSubfolders")
    List("sub/c.js", "sub/d.js", "a.js", "b.js").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      file.write(s"""console.log("${file.canonicalPath}");""")
    }
    dir
  }

  private val projectWithBrokenFile: File = {
    val dir      = File.newTemporaryDirectory("jssrc2cpgTestsBroken")
    val goodFile = dir / "good.js"
    goodFile.createIfNotExists(createParents = true)
    goodFile.write(s"""console.log("good");""")
    val brokenFile = dir / "broken.js"
    brokenFile.createIfNotExists(createParents = true)
    brokenFile.write(s"""console.log("broken""")
    dir
  }

  private val projectWithUtf8: File = {
    val dir  = File.newTemporaryDirectory("jssrc2cpgTestsUtf8")
    val file = dir / "utf8.js"
    file.createIfNotExists(createParents = true)
    file.write("""
        |// 😼
        |logger.error()
        |""".stripMargin)
    dir
  }

  private val projectWithStrangeFilenames: File = {
    val dir = File.newTemporaryDirectory("jssrc2cpgTestsFilenames")
    List("good_%component-name%_.js", "good_%component-name%_Foo.js").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      file.write(s"""console.log("${file.canonicalPath}");""")
    }
    List("broken_%component-name%_.js", "broken_%component-name%_Foo.js").foreach { testFile =>
      val file = dir / testFile
      file.createIfNotExists(createParents = true)
      file.write(s"""const x = new <%ComponentName%>Foo();""")
    }
    dir
  }

  override def afterAll(): Unit = {
    projectWithSubfolders.delete(swallowIOExceptions = true)
    projectWithBrokenFile.delete(swallowIOExceptions = true)
    projectWithUtf8.delete(swallowIOExceptions = true)
    projectWithStrangeFilenames.delete(swallowIOExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: File)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { tmpDir =>
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
