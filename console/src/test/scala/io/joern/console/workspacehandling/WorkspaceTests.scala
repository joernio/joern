package io.joern.console.workspacehandling

import io.joern.console.testing.availableWidthProvider
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.testing.MockCpg
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer

class WorkspaceTests extends AnyWordSpec with Matchers {

  "toString" should {

    "return an \"empty\" when no projects are present" in {
      val workspace = new Workspace(ListBuffer())
      workspace.toString shouldBe "empty"
    }

    "return a valid row for a project" in {
      FileUtil.usingTemporaryDirectory("project") { project =>
        Files.createDirectory(project / "overlays")
        val inputPath   = "/input/path"
        val projectFile = ProjectFile(inputPath, project.fileName)
        val cpg         = MockCpg().withMetaData("C", List("foo", "bar")).cpg
        val projects    = ListBuffer(Project(projectFile, project, Some(cpg)))
        val workspace   = new Workspace(projects)
        val output      = workspace.toString

        output should include(project.fileName)
        output should include(inputPath)

        // This relies on the file system and only works in a staged joern environment, not our workspace
        // with a mock cpg. Will leave here just for illustratory purposes.
//        output should include("foo,bar")
      }
    }

  }

}

object WorkspaceTests {

  def createFakeProject(workspaceFile: Path, projectName: String): Path = {
    Files.createDirectory(workspaceFile / projectName)
    Files.createDirectory(workspaceFile / projectName / "overlays")

    val projectJson = (workspaceFile / projectName / "project.json")
    val jsonContent = s"""{"inputPath":"foo","name":"$projectName"}"""
    Files.writeString(projectJson, jsonContent)

    (workspaceFile / projectName / "cpg.bin").createWithParentsIfNotExists()
  }

}
