package io.joern.console.workspacehandling

import better.files.*
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WorkspaceManagerTests extends AnyWordSpec with Matchers {

  private val tmpDirPrefix = "workspace-tests"

  "WorkspaceManager" should {

    "create project in correct location" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val inputFile     = File.newTemporaryFile("workspaceman").touch()
        val workspacePath = workspaceFile.path
        val manager       = new WorkspaceManager(workspacePath.toString)
        val pathToProject = manager.createProject(inputFile.toString, "aprojectname")
        pathToProject shouldBe Some(workspacePath.resolve("aprojectname"))
        inputFile.delete()
        manager.numberOfProjects shouldBe 1
      }
    }

    "overwrite existing project with same name" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val workspacePath = workspaceFile.path
        val manager       = new WorkspaceManager(workspacePath.toString)
        val inputFile1    = File.newTemporaryFile("workspaceman").touch()
        val inputFile2    = File.newTemporaryFile("workspaceman").touch()
        manager.createProject(inputFile1.toString, "aprojectname")
        val pathToProject = manager.createProject(inputFile2.toString, "aprojectname")
        val project       = pathToProject.flatMap(TestLoader().loadProject(_)).get
        inputFile1.delete()
        inputFile2.delete()
        File(project.inputPath).name shouldBe inputFile2.name
        manager.numberOfProjects shouldBe 1
      }
    }

    "not create project if input path is invalid" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val workspacePath = workspaceFile.path
        val manager       = new WorkspaceManager(workspacePath.toString)
        val project       = manager.createProject("idonotexist", "aprojectname")
        project shouldBe None
        manager.numberOfProjects shouldBe 0
      }
    }

    "correctly reset the workspace" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { tmpDir =>
        WorkspaceTests.createFakeProject(tmpDir, "1")
        val manager = new WorkspaceManager(tmpDir.toString)
        manager.numberOfProjects shouldBe 1
        manager.reset
        manager.numberOfProjects shouldBe 0
      }
    }

    "return None when asking to load non-existing project" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val workspacePath = workspaceFile.path
        val project       = new WorkspaceManager(workspacePath.toString).openProject("foo")
        project shouldBe None
      }
    }

    "open an existing project" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
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
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
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
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        val manager     = createFakeProjectAndOpen(workspaceFile, projectName)
        val project     = manager.closeProject(projectName)
        project.get.name shouldBe projectName
        project.get.cpg shouldBe None
        manager.numberOfProjects shouldBe 1
      }
    }

    "gracefully handle closing of a closed project" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        val manager     = createFakeProjectAndOpen(workspaceFile, projectName)
        manager.closeProject(projectName)
        manager.closeProject(projectName)
      }
    }

    "handle corrupt CPGs gracefully" in {
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
        val projectName = "myproject"
        WorkspaceTests.createFakeProject(workspaceFile, projectName)
        (workspaceFile.toString / projectName / "cpg.bin").write("corrupt")
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
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
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
      File.usingTemporaryDirectory(tmpDirPrefix) { workspaceFile =>
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

    def createFakeProjectAndOpen(workspaceFile: File, projectName: String): WorkspaceManager[Project] = {
      WorkspaceTests.createFakeProject(workspaceFile, projectName)
      val manager = new WorkspaceManager[Project](workspaceFile.toString)
      manager.openProject(projectName, (_: String) => Some(Cpg.empty))
      manager
    }

  }

}
