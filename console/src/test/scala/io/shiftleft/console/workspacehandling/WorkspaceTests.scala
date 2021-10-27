package io.shiftleft.console.workspacehandling

import better.files.Dsl._
import better.files.File
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
        val inputPath = "/input/path"
        val projectFile = ProjectFile(inputPath, project.name)
        val cpg = MockCpg().withMetaData("C", List("foo", "bar")).cpg
        val projects = ListBuffer(
          Project(projectFile, project.path, Some(cpg))
        )
        val workspace = new Workspace(projects)
        val output = workspace.toString
        val lines = output.split("\n")
        lines.length shouldBe 5
        lines(4).contains(project.name) shouldBe true
        lines(4).contains(inputPath)
        lines(4).contains("foo,bar")
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
