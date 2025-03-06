package io.joern.console.workspacehandling

import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*

import java.nio.file.Files
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WorkspaceLoaderTests extends AnyWordSpec with Matchers {

  private val tmpDirPrefix = "workspace-tests"

  "WorkspaceLoader" should {

    "create workspace and workspace directory if nonexistent" in {
      val dir = Files.createTempDirectory(tmpDirPrefix)
      FileUtil.delete(dir)
      TestLoader().load(dir.toString)
      try {
        Files.exists(dir) shouldBe true
      } finally {
        FileUtil.delete(dir)
      }
    }

    "handle broken project.json gracefully by skipping project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        Files.createDirectory(tmpDir / "1")
        val jsonPath = (tmpDir / "1" / "project.json")
        Files.writeString(jsonPath, "{foo")
        TestLoader().load(tmpDir.toString).numberOfProjects shouldBe 0
      }
    }

    "load project correctly" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        val projectName = "foo"
        WorkspaceTests.createFakeProject(tmpDir, projectName)
        val project = TestLoader().loadProject((tmpDir / projectName))
        project match {
          case Some(p) =>
            p.name shouldBe "foo"
            p.inputPath shouldBe "foo"
            p.cpg shouldBe None
          case None => fail()
        }
      }
    }

    "initialize workspace's project list correctly" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        val projectName = "foo"
        WorkspaceTests.createFakeProject(tmpDir, projectName)
        val workspace = TestLoader().load(tmpDir.toString)
        workspace.numberOfProjects shouldBe 1
      }
    }
  }

  "ProjectFile" should {

    import org.json4s.DefaultFormats
    import org.json4s.native.Serialization.{read => jsonRead, write => jsonWrite}
    implicit val formats: DefaultFormats.type = DefaultFormats

    "be serializable to json" in {
      jsonWrite(ProjectFile("foo", "aname")) shouldBe """{"inputPath":"foo","name":"aname"}"""
    }

  }

}
