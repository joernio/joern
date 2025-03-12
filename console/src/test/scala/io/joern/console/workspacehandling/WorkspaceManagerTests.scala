package io.joern.console.workspacehandling

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path, Paths}

class WorkspaceManagerTests extends AnyWordSpec with Matchers {

  private val tmpDirPrefix = "workspace-tests"

  "WorkspaceManager" should {

    "create project in correct location" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val inputFile     = FileUtil.newTemporaryFile("workspaceman")
        val manager       = new WorkspaceManager(workspaceFile.toString)
        val pathToProject = manager.createProject(inputFile.toString, "aprojectname")
        pathToProject shouldBe Some(workspaceFile.resolve("aprojectname"))
        FileUtil.delete(inputFile)
        manager.numberOfProjects shouldBe 1
      }
    }

    "overwrite existing project with same name" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val manager    = new WorkspaceManager(workspaceFile.toString)
        val inputFile1 = FileUtil.newTemporaryFile("workspaceman")
        val inputFile2 = FileUtil.newTemporaryFile("workspaceman")
        manager.createProject(inputFile1.toString, "aprojectname")
        val pathToProject = manager.createProject(inputFile2.toString, "aprojectname")
        val project       = pathToProject.flatMap(TestLoader().loadProject(_)).get
        FileUtil.delete(inputFile1)
        FileUtil.delete(inputFile2)
        Paths.get(project.inputPath).fileName shouldBe inputFile2.fileName
        manager.numberOfProjects shouldBe 1
      }
    }

    "not create project if input path is invalid" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val manager = new WorkspaceManager(workspaceFile.toString)
        val project = manager.createProject("idonotexist", "aprojectname")
        project shouldBe None
        manager.numberOfProjects shouldBe 0
      }
    }

    "correctly reset the workspace" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        WorkspaceTests.createFakeProject(tmpDir, "1")
        val manager = new WorkspaceManager(tmpDir.toString)
        manager.numberOfProjects shouldBe 1
        manager.reset
        manager.numberOfProjects shouldBe 0
      }
    }

    "return None when asking to load non-existing project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val project = new WorkspaceManager(workspaceFile.toString).openProject("foo")
        project shouldBe None
      }
    }

    "open an existing project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        val manager     = createFakeProjectAndOpen(workspaceFile, projectName)
        manager.numberOfProjects shouldBe 1
        val project = manager.project(projectName)

        project match {
          case Some(p) =>
            p.name shouldBe projectName
            p.cpg should not be empty
          case _ => fail()
        }
      }
    }

    "automatically create and open a cpg working copy on open" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        WorkspaceTests.createFakeProject(workspaceFile, projectName)
        val manager = new WorkspaceManager[Project](workspaceFile.toString)
        manager.openProject(
          projectName,
          (fileName: String) => {
            fileName.endsWith("cpg.bin.tmp") shouldBe true
            Some(Cpg.empty)
          }
        )

        val project = manager.project(projectName)
        project match {
          case Some(p) =>
            p.path.resolve("cpg.bin").toFile.exists() shouldBe true
            p.path.resolve("cpg.bin.tmp").toFile.exists() shouldBe true
          case _ => fail()
        }
      }
    }

    "allow closing an open project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        val manager     = createFakeProjectAndOpen(workspaceFile, projectName)
        val project     = manager.closeProject(projectName)
        project.get.name shouldBe projectName
        project.get.cpg shouldBe None
        manager.numberOfProjects shouldBe 1
      }
    }

    "gracefully handle closing of a closed project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        val manager     = createFakeProjectAndOpen(workspaceFile, projectName)
        manager.closeProject(projectName)
        manager.closeProject(projectName)
      }
    }

    "handle corrupt CPGs gracefully" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        WorkspaceTests.createFakeProject(workspaceFile, projectName)
        val cpgPath = (workspaceFile / projectName / "cpg.bin")
        Files.writeString(cpgPath, "corrupt")
        val manager = new WorkspaceManager(workspaceFile.toString)
        manager.openProject(projectName) shouldBe None
        manager.numberOfProjects shouldBe 1
        manager.project(projectName) match {
          case Some(p) =>
            p.name shouldBe projectName
            p.cpg shouldBe None
          case None => fail()
        }
        a[RuntimeException] should be thrownBy manager.cpg
      }
    }

    "allow setting and retrieving the active project" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        WorkspaceTests.createFakeProject(workspaceFile, projectName)
        val projectName2 = "myproject2"
        WorkspaceTests.createFakeProject(workspaceFile, projectName2)
        val manager = new WorkspaceManager(workspaceFile.toString)
        manager.setActiveProject(projectName2)
        manager.getActiveProject.exists(p => p.name == projectName2) shouldBe true
        manager.setActiveProject(projectName)
        manager.getActiveProject.exists(p => p.name == projectName) shouldBe true
        manager.getActiveProject.exists(p => p.name == projectName2) shouldBe false
      }
    }

    "disallow setting the active project to a non-existing project`" in {
      FileUtil.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        WorkspaceTests.createFakeProject(workspaceFile, projectName)
        val manager = new WorkspaceManager(workspaceFile.toString)
        manager.openProject(projectName)
        val cpgBeforeCall = manager.cpg
        val project       = manager.setActiveProject("idontexist")
        project shouldBe None
        manager.cpg shouldBe cpgBeforeCall
        manager.getActiveProject should not be empty
      }
    }

    def createFakeProjectAndOpen(workspaceFile: Path, projectName: String): WorkspaceManager[Project] = {
      WorkspaceTests.createFakeProject(workspaceFile, projectName)
      val manager = new WorkspaceManager[Project](workspaceFile.toString)
      manager.openProject(projectName, (_: String) => Some(Cpg.empty))
      manager
    }

  }

}
