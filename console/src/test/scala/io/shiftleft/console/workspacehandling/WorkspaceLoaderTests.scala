package io.shiftleft.console.workspacehandling

import better.files.Dsl.mkdir
import better.files.File
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.io.Directory

class WorkspaceLoaderTests extends AnyWordSpec with Matchers {

  private val tmpDirPrefix = "workspace-tests"

  "WorkspaceLoader" should {

    "create workspace and workspace directory if nonexistent" in {
      val dir = File.newTemporaryDirectory(tmpDirPrefix)
      new Directory(dir.toJava).deleteRecursively()
      TestLoader().load(dir.path.toString)
      try {
        dir.exists shouldBe true
      } finally {
        new Directory(dir.toJava).deleteRecursively()
      }
    }

    "handle broken project.json gracefully by skipping project" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        mkdir(tmpDir / "1")
        (tmpDir / "1" / "project.json").write("{foo")
        TestLoader().load(tmpDir.path.toString).numberOfProjects shouldBe 0
      }
    }

    "load project correctly" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        val projectName = "foo"
        WorkspaceTests.createFakeProject(tmpDir, projectName)
        val project = TestLoader().loadProject((tmpDir / projectName).path)
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
      File.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
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

    "be deserializable from json" in {
      val projectFile = jsonRead[ProjectFile]("""{"inputPath":"foo","name":"aname"}""")
      projectFile.inputPath shouldBe "foo"
      projectFile.name shouldBe "aname"
    }

  }

}
