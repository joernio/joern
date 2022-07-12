package io.joern.jssrc2cpg.io

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ProjectParseTests extends AnyWordSpec with Matchers {

  private object ProjectParseTestsFixture {
    def apply(project: String)(f: File => Unit): Unit = {
      val projectPath = getClass.getResource(s"/$project").toURI
      File.usingTemporaryDirectory()(tmpDir => f(File(projectPath).copyToDirectory(tmpDir)))
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture("rec") { tmpDir =>
      val cpg = new JsSrc2CpgFrontend().execute(tmpDir.toJava)
      cpg.file.name.l should contain allElementsOf List(
        "a.js",
        "b.js",
        s"sub${java.io.File.separator}c.js",
        s"sub${java.io.File.separator}d.js"
      )
    }

    "recover from broken input file" in ProjectParseTestsFixture("broken") { tmpDir =>
      val cpg = new JsSrc2CpgFrontend().execute(tmpDir.toJava)
      cpg.file.name.l should (contain("good.js") and not contain ("broken.js"))
    }

  }

}
