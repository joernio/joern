package io.joern.jssrc2cpg.parser

import better.files.File
import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgFrontend
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.TraversalSource
import overflowdb.traversal.toElementTraversal

class ProjectParseTests extends AnyWordSpec with Matchers {
  private def fileNames(cpg: Cpg): List[String] = {
    val result =
      TraversalSource(cpg.graph).label(NodeTypes.FILE).property(PropertyNames.NAME).toList
    result.size should not be 0
    result
  }

  private object ProjectParseTestsFixture {
    def apply(project: String)(f: File => Unit): Unit = {
      val projectPath = getClass.getResource(s"/$project").toURI
      File.usingTemporaryDirectory()(tmpDir => f(File(projectPath).copyToDirectory(tmpDir)))
    }
  }

  "Parsing a project" should {

    "generate correct filenames" in ProjectParseTestsFixture("rec") { tmpDir =>
      val cpg = new JsSrc2CpgFrontend().execute(tmpDir.toJava)
      fileNames(cpg) should contain allElementsOf List(
        "a.js",
        "b.js",
        s"sub${java.io.File.separator}c.js",
        s"sub${java.io.File.separator}d.js"
      )
    }

    "recover from broken input file" in ProjectParseTestsFixture("broken") { tmpDir =>
      val cpg = new JsSrc2CpgFrontend().execute(tmpDir.toJava)
      fileNames(cpg) should not contain "broken.js"
      fileNames(cpg) should contain("good.js")
    }

  }

}
