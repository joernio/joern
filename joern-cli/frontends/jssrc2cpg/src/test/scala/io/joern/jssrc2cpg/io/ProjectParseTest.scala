package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll

class ProjectParseTest extends JsSrc2CpgSuite with BeforeAndAfterAll {

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

  override def afterAll(): Unit = {
    projectWithSubfolders.delete(swallowIOExceptions = true)
    projectWithBrokenFile.delete(swallowIOExceptions = true)
  }

  private object ProjectParseTestsFixture {
    def apply(projectDir: File)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { tmpDir =>
        val cpg          = newEmptyCpg()
        val config       = Config(inputPath = projectDir.toString, outputPath = tmpDir.toString)
        val astgenResult = AstGenRunner.execute(config, tmpDir)
        new AstCreationPass(cpg, astgenResult, config).createAndApply()
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

  }

}
