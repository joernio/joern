package io.joern.jssrc2cpg.parsetests

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoader
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.TraversalSource
import overflowdb.Config

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
      val cpgPath = (tmpDir / "cpg.bin").path.toString
      JsSrc2Cpg.main(Array(tmpDir.pathAsString, "--output", cpgPath))

      val cpg =
        CpgLoader
          .loadFromOverflowDb(
            CpgLoaderConfig.withDefaults
              .withOverflowConfig(Config.withDefaults.withStorageLocation(cpgPath))
          )

      fileNames(cpg) shouldBe List(
        "a.js",
        "b.js",
        s"sub${java.io.File.separator}c.js",
        s"sub${java.io.File.separator}d.js"
      )
    }

  }

}
