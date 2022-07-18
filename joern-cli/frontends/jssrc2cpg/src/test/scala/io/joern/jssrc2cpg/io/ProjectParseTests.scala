package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

class ProjectParseTests extends JsSrc2CpgSuite {

  private object ProjectParseTestsFixture {
    def apply(project: String)(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTests") { tmpDir =>
        val dir          = File(getClass.getResource(s"/$project").toURI).copyToDirectory(tmpDir)
        val cpg          = newEmptyCpg()
        val config       = Config(inputPath = dir.toString, outputPath = dir.toString)
        val astgenResult = AstGenRunner.execute(config, dir)
        new AstCreationPass(cpg, astgenResult, config).createAndApply()
        f(cpg)
      }
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture("rec") { cpg =>
      cpg.file.name.l should contain allElementsOf List(
        "a.js",
        "b.js",
        s"sub${java.io.File.separator}c.js",
        s"sub${java.io.File.separator}d.js"
      )
    }

    "recover from broken input file" in ProjectParseTestsFixture("broken") { cpg =>
      cpg.file.name.l should (contain("good.js") and not contain ("broken.js"))
    }

  }

}
