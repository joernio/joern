package io.joern.console

import better.files.File
import io.joern.console.testing.*
import io.joern.x2cpg.X2Cpg.defaultOverlayCreators
import io.joern.x2cpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Ignore
import org.scalatest.Tag

import java.nio.file.{Files, Path, Paths}
import java.io.{BufferedInputStream, BufferedOutputStream, FileOutputStream, StreamTokenizer}
import java.util.zip.ZipOutputStream
import scala.util.{Properties, Try, Using, Success, Failure}

class ConsoleTests extends AnyWordSpec with Matchers {

  // Some tests here are are copying stuff within TEMP which is not allowed within the Windows GITHUB actions runners.
  private object NotInWindowsRunners
      extends Tag(
        if (!Paths.get(Properties.tmpDir).toString.contains(":\\Users\\RUNNER~1\\AppData\\Local\\Temp")) ""
        else classOf[Ignore].getName
      )

  "importCode" should {
    "warn about non-existent dir" in ConsoleFixture() { (console, _) =>
      val nonExistentDir = "/does/not/exist/"
      intercept[ConsoleException] {
        console.importCode(nonExistentDir)
      }.getMessage shouldBe s"Input path does not exist: '$nonExistentDir'"
      intercept[ConsoleException] {
        console.importCode.c(nonExistentDir)
      }.getMessage shouldBe s"Input path does not exist: '$nonExistentDir'"
      intercept[ConsoleException] {
        console.importCode.jssrc(nonExistentDir)
      }.getMessage shouldBe s"Input path does not exist: '$nonExistentDir'"
      intercept[ConsoleException] {
        console.importCode.swiftsrc(nonExistentDir)
      }.getMessage shouldBe s"Input path does not exist: '$nonExistentDir'"
      intercept[ConsoleException] {
        console.importCode.java(nonExistentDir)
      }.getMessage shouldBe s"Input path does not exist: '$nonExistentDir'"
    }

    "provide overview of available language modules" in ConsoleFixture() { (console, _) =>
      console.importCode.toString should include("testCFrontend")
    }

    "allow importing code with specific module (c2cpg)" in ConsoleFixture() { (console, codeDir) =>
      console.importCode.c(codeDir.toString)
      console.workspace.numberOfProjects shouldBe 1
    }

    "allow importing code" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString)
      console.workspace.numberOfProjects shouldBe 1
      Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
      console.project.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
      console.project.availableOverlays.toSet shouldBe Set(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
    }

    "allow importing code from file with defines and additional args" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        val code =
          """
          |#ifdef D
          |int foo() {};
          |#endif
          |""".stripMargin
        FileUtil.usingTemporaryFile("console", suffix = ".c", parent = Option(codeDir)) { file =>
          Files.writeString(file, code)
          console.importCode.c(inputPath = codeDir.toString)
          // importing without args should not yield foo
          Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe false

          // importing with args should yield foo
          console.importCode.c(inputPath = codeDir.toString(), args = List("--define", "D"))
          Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
        }
    }

    "allow importing code from string with defines and additional args" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, _) =>
        val code =
          """
          |#ifdef D
          |int foo() {};
          |#endif
          |""".stripMargin
        // importing without args should not yield foo
        console.importCode.c.fromString(code)
        Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe false

        // importing with args should yield foo
        console.importCode.c.fromString(code, List("--define", "D"))
        Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
    }

    "allow importing code from file with JS frontend via apply" in ConsoleFixture() { (console, _) =>
      val code = "function foo() {};"
      FileUtil.usingTemporaryFile("consoleTests", ".js") { tmpFile =>
        Files.writeString(tmpFile, code)
        console.importCode(tmpFile.toString)
        Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
      }
    }

    "allow importing code from file with JS frontend" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, _) =>
      val code = "function foo() {};"
      FileUtil.usingTemporaryFile("consoleTests", ".js") { tmpFile =>
        Files.writeString(tmpFile, code)
        console.importCode.jssrc(tmpFile.toString)
        Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
      }
    }

    "allow importing code from file with Swift frontend via apply" in ConsoleFixture() { (console, _) =>
      val code = "func foo() {};"
      FileUtil.usingTemporaryFile("consoleTests", ".swift") { tmpFile =>
        Files.writeString(tmpFile, code)
        console.importCode(tmpFile.toString)
        Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
      }
    }

    "allow importing code from file with Swift frontend" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, _) =>
        val code = "func foo() {};"
        FileUtil.usingTemporaryFile("consoleTests", ".swift") { tmpFile =>
          Files.writeString(tmpFile, code)
          console.importCode.swiftsrc(tmpFile.toString)
          Set("foo").subsetOf(console.cpg.method.name.toSet) shouldBe true
        }
    }

    "allow importing code and setting project name" in ConsoleFixture() { (console, codeDir) =>
      val projectName = "test"
      console.importCode(codeDir.toString, projectName)
      console.workspace.numberOfProjects shouldBe 1
      console.workspace.project(projectName) match {
        case Some(p) =>
          p.name shouldBe projectName
          p.cpg should not be empty
        case None => fail()
      }
      console.project.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
    }

    "allow importing multiple code bases" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString, "foo")
      console.importCode(codeDir.toString, "bar")
      console.workspace.numberOfProjects shouldBe 2
      console.project.name shouldBe "bar"
      console.project.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
      console.workspace.project("foo").get.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
    }

    "set project to active" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString, "foo")
      Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
    }

  }

  "importCpg" should {

    "gracefully handle request to import non-existing CPG" in ConsoleFixture() { (console, _) =>
      console.importCpg("idontexist")
      console.workspace.numberOfProjects shouldBe 0
    }

    "convert legacy CPGs on import" in ConsoleFixture() { (console, _) =>
      FileUtil.usingTemporaryFile("console") { file =>
        new ZipOutputStream(new FileOutputStream(file.toString)).close()
        val projectName = "myproject"
        val cpg         = console.importCpg(file.toString, projectName)
        console.workspace.numberOfProjects shouldBe 1
        console.workspace.projectByCpg(cpg.get).map(_.name) shouldBe Option(projectName)
        console.project.appliedOverlays shouldBe List()
      }
    }

    "handle broken legacy CPG gracefully" in ConsoleFixture() { (console, _) =>
      FileUtil.usingTemporaryFile("console") { file =>
        Files.writeString(file, "PK")
        console.importCpg(file.toString)
      }
    }

    "gracefully handle broken CPGs" in ConsoleFixture() { (console, _) =>
      FileUtil.usingTemporaryFile("console") { file =>
        FileUtil.writeBytes(file, List[Byte]('F', 'O'))
        console.importCpg(file.toString)
        console.workspace.numberOfProjects shouldBe 0
      }
    }

    "allow importing an existing CPG" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      WithStandaloneCpg(console, codeDir) { tmpCpg =>
        console.importCpg(tmpCpg.toString)
        console.workspace.numberOfProjects shouldBe 1
        Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
        console.project.appliedOverlays shouldBe List(
          Base.overlayName,
          ControlFlow.overlayName,
          TypeRelations.overlayName,
          CallGraph.overlayName
        )
      }
    }

    "allow importing an existing CPG with custom project name" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        WithStandaloneCpg(console, codeDir) { tmpCpg =>
          console.importCpg(tmpCpg.toString, "foobar")
          console.workspace.numberOfProjects shouldBe 1
          console.workspace.project("foobar") should not be empty
          Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
        }
    }

    "allow importing two CPGs with the same filename but different paths" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        WithStandaloneCpg(console, codeDir) { tmpCpg =>
          FileUtil.usingTemporaryDirectory("console") { dir1 =>
            FileUtil.usingTemporaryDirectory("console") { dir2 =>
              FileUtil.usingTemporaryDirectory("console") { dir3 =>
                val cpg1Path = dir1.resolve("cpg.bin")
                val cpg2Path = dir2.resolve("cpg.bin")
                val cpg3Path = dir3.resolve("cpg.bin")
                FileUtil.copyFiles(tmpCpg, cpg1Path)
                FileUtil.copyFiles(tmpCpg, cpg2Path)
                FileUtil.copyFiles(tmpCpg, cpg3Path)
                console.importCpg(cpg1Path.toString)
                console.importCpg(cpg2Path.toString)
                console.importCpg(cpg3Path.toString)
                console.workspace.numberOfProjects shouldBe 3
                console.workspace.project(cpg1Path.toFile.getName) should not be empty
                console.workspace.project(s"${cpg1Path.toFile.getName}1") should not be empty
                console.workspace.project(s"${cpg1Path.toFile.getName}2") should not be empty
                console.workspace.project(s"${cpg1Path.toFile.getName}12") shouldBe empty
              }
            }
          }
        }
    }

    "overwrite project if a project for the inputPath exists" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        WithStandaloneCpg(console, codeDir) { tmpCpg =>
          console.importCpg(tmpCpg.toString)
          console.importCpg(tmpCpg.toString)
          console.workspace.numberOfProjects shouldBe 1
          Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
          console.project.appliedOverlays shouldBe List(
            Base.overlayName,
            ControlFlow.overlayName,
            TypeRelations.overlayName,
            CallGraph.overlayName
          )
        }
    }
  }

  "open/close" should {
    "allow opening an already open project to make it active" in ConsoleFixture() { (console, codeDir) =>
      val projectName = "myproject"
      console.importCode(codeDir.toString, projectName)
      console.importCpg(codeDir.resolve("cpg.bin").toString, "foo")
      val project = console.open(projectName)
      project match {
        case Some(p) =>
          p.name shouldBe projectName
          console.workspace.projectByCpg(p.cpg.get).map(_.name) shouldBe Option(projectName)
        case None => fail()
      }
    }

    "allow closing and then opening a project again" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        val projectName = "myproject"
        console.importCode(codeDir.toString, projectName)
        console.close(projectName).get.cpg shouldBe empty
        console.open(projectName).get.cpg should not be empty
    }

    "allow closing currently active project" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      val projectName = "myproject"
      console.importCode(codeDir.toString, projectName)
      val project = console.close
      project.get.name shouldBe projectName
      project.get.cpg shouldBe empty
    }

    "keep project active on close and allow setting other as active" taggedAs NotInWindowsRunners in ConsoleFixture() {
      (console, codeDir) =>
        console.importCode(codeDir.toString, "foo")
        console.importCode(codeDir.toString, "bar")
        console.close
        a[RuntimeException] should be thrownBy console.cpg
        console.open("foo")
        Set("main", "bar").subsetOf(console.cpg.method.name.toSet) shouldBe true
    }
  }

  "delete" should {

    "remove a project from disk" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      val cpg        = console.importCode(codeDir.toString, "foo")
      val projectDir = console.workspace.projectByCpg(cpg).get.path.toFile
      console.delete("foo")
      projectDir.exists shouldBe false
    }

    "handle request to delete non-existing project gracefully" in ConsoleFixture() { (console, _) =>
      val project = console.delete("foo")
      project shouldBe None
    }

  }

  "runAnalyzer" should {

    "prohibit adding semanticcpg again" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString)
      console.project.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
      val numOverlayFilesBefore = console.project.path.resolve("overlays").toFile.list().length
      numOverlayFilesBefore shouldBe 4
      console._runAnalyzer(defaultOverlayCreators()*)
      console.project.appliedOverlays shouldBe List(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )
      console.project.path.resolve("overlays").toFile.list().length shouldBe numOverlayFilesBefore
    }

    "store directory of zip files for each overlay in project" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString)
      val overlayParentDir = console.project.path.resolve("overlays")
      overlayParentDir.toFile.list.toSet shouldBe Set(
        Base.overlayName,
        ControlFlow.overlayName,
        TypeRelations.overlayName,
        CallGraph.overlayName
      )

      val overlayDirs = overlayParentDir.toFile.listFiles()
      overlayDirs.foreach { dir =>
        Files.isDirectory(Paths.get(dir.getPath)) shouldBe true
        Paths.get(dir.getPath).listFiles().foreach { file =>
          Try { file.getFileName.toString.split("_").head.toInt }.isSuccess shouldBe true
          isZipFile(file) shouldBe true
        }
      }
    }
  }

  private def isZipFile(file: Path): Boolean = {
    Using.Manager { use =>
      val fis = use(Files.newInputStream(file))
      val bis = use(new BufferedInputStream(fis))

      Iterator.continually(bis.read()).takeWhile(_ != StreamTokenizer.TT_EOF).map(_.toByte)
    } match {
      case Success(bytes) =>
        Try {
          bytes.next() == 'P' && bytes.next() == 'K'
        }.getOrElse(false)
      case Failure(_) =>
        false
    }
  }

  class MockLayerCreator extends LayerCreator {
    override val overlayName: String = "fooname"
    override val description: String = "foodescr"

    override def create(context: LayerCreatorContext): Unit = {}
  }

  "save" should {
    "close and reopen projects" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString, "project1")
      console.importCode(codeDir.toString, "project2")
      console.workspace.project("project1").exists(_.cpg.isDefined) shouldBe true
      console.workspace.project("project2").exists(_.cpg.isDefined) shouldBe true
      console.save
      console.workspace.project("project1").exists(_.cpg.isDefined) shouldBe true
      console.workspace.project("project2").exists(_.cpg.isDefined) shouldBe true
    }

    "copy working copy to persistent copy" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString, "project1")
      val projectPath = console.project.path
      console.save
      val persistentCpgSize = projectPath.resolve("cpg.bin").toFile.length()
      val workingCpgSize    = projectPath.resolve("cpg.bin.tmp").toFile.length()
      persistentCpgSize shouldBe workingCpgSize
    }

  }

  "cpg" should {
    "provide .help command" in ConsoleFixture() { (console, codeDir) =>
      // part of Predefined.shared, which makes the below work in the repl without separate import
      import io.shiftleft.semanticcpg.language.docSearchPackages
      import io.joern.console.testing.availableWidthProvider

      console.importCode(codeDir.toString)
      val nodeStartersHelp = console.cpg.help
      nodeStartersHelp should include(".all")
      nodeStartersHelp should include(".controlStructure")

      val methodStepsHelp = console.cpg.method.help
      methodStepsHelp should include(".namespace")
    }
  }

  "runCustomQuery" should {
    "allow running a custom query" in ConsoleFixture() { (console, codeDir) =>
      console.importCode(codeDir.toString)
      Run.runCustomQuery(console, console.cpg.method.newTagNode("mytag"))
      console.cpg.tag.name("mytag").method.name.toSet should contain("main")
      console.cpg.metaData.map(_.overlays).head.last shouldBe "custom"
    }
  }

  "switchWorkspace" should {

    "create workspace if directory does not exist" in ConsoleFixture() { (console, _) =>
      val otherWorkspaceDir = Paths.get(Properties.tmpDir) / "workspace-doesNotExist"
      try {
        Files.exists(otherWorkspaceDir) shouldBe false
        console.switchWorkspace(otherWorkspaceDir.toString)
        Files.exists(otherWorkspaceDir) shouldBe true
      } finally {
//        otherWorkspaceDir.delete()
//        otherWorkspaceDir.exists shouldBe false
        FileUtil.delete(otherWorkspaceDir)
        Files.exists(otherWorkspaceDir) shouldBe false
      }
    }

    "allow changing workspaces" taggedAs NotInWindowsRunners in ConsoleFixture() { (console, codeDir) =>
      val firstWorkspace = Paths.get(console.workspace.getPath)

      FileUtil.usingTemporaryDirectory("console") { otherWorkspaceDir =>
        console.importCode(codeDir.toString, "projectInFirstWorkspace")
        console.workspace.numberOfProjects shouldBe 1
        console.switchWorkspace(otherWorkspaceDir.toString)
        console.workspace.numberOfProjects shouldBe 0

        console.importCode(codeDir.toString, "projectInSecondWorkspace")
        console.workspace.numberOfProjects shouldBe 1
        console.project.name shouldBe "projectInSecondWorkspace"
        console.switchWorkspace(firstWorkspace.toString)
        console.workspace.numberOfProjects shouldBe 1
        console.open("projectInFirstWorkspace")
        console.project.name shouldBe "projectInFirstWorkspace"
      }
    }
  }

}
