package io.joern.console.workspacehandling

import better.files.Dsl.*
import better.files.File
import io.joern.console.testing.availableWidthProvider
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class WorkspaceTests extends AnyWordSpec with Matchers {

  "toString" should {

    "return an \"empty\" when no projects are present" in {
      val workspace = new Workspace(ListBuffer())
      workspace.toString shouldBe "empty"
    }

    "return a valid row for a project" in {
      File.usingTemporaryDirectory("project") { project =>
        mkdir(project / "overlays")
        val inputPath   = "/input/path"
        val projectFile = ProjectFile(inputPath, project.name)
        val cpg         = MockCpg().withMetaData("C", List("foo", "bar")).cpg
        val projects    = ListBuffer(Project(projectFile, project.path, Some(cpg)))
        val workspace   = new Workspace(projects)
        val output      = workspace.toString

        output should include(project.name)
        output should include(inputPath)

        // This relies on the file system and only works in a staged joern environment, not our workspace
        // with a mock cpg. Will leave here just for illustratory purposes.
//        output should include("foo,bar")
      }
    }

  }

}

object WorkspaceTests {

  def createFakeProject(workspaceFile: File, projectName: String): File = {
    mkdir(workspaceFile / projectName)
    mkdir(workspaceFile / projectName / "overlays")
    (workspaceFile / projectName / "project.json")
      .write(s"""{"inputPath":"foo","name":"$projectName"}""")
    touch(workspaceFile / projectName / "cpg.bin")
  }

}
