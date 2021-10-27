package io.joern.console.testing

import better.files.Dsl.mkdir
import better.files.File
import io.shiftleft.console.cpgcreation.{CpgGenerator, CpgGeneratorFactory, ImportCode}
import io.shiftleft.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.shiftleft.console.{Console, ConsoleConfig, DefaultAmmoniteExecutor, InstallConfig}
import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg

import java.nio.file.Path

object ConsoleFixture {
  def apply[T <: Console[Project]](constructor: String => T = { x =>
    new TestConsole(x)
  })(fun: (T, File) => Unit): Unit = {
    File.usingTemporaryDirectory("console") { workspaceDir =>
      File.usingTemporaryDirectory("console") { codeDir =>
        mkdir(codeDir / "dir1")
        mkdir(codeDir / "dir2")
        (codeDir / "dir1" / "foo.c")
          .write("int main(int argc, char **argv) { char *ptr = 0x1 + argv; return argc; }")
        (codeDir / "dir2" / "bar.c").write("int bar(int x) { return x; }")
        val console = constructor(workspaceDir.toString)
        fun(console, codeDir)
      }
    }
  }

}

object TestWorkspaceLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = Project(projectFile, path)
}

class TestConsole(workspaceDir: String)
    extends Console[Project](DefaultAmmoniteExecutor, TestWorkspaceLoader, File(workspaceDir)) {
  override def config = new ConsoleConfig(
    install = new InstallConfig(Map("SHIFTLEFT_OCULAR_INSTALL_DIR" -> workspaceDir))
  )

  class MyImportCode[T <: Project](console: Console[T]) extends ImportCode(console) {
    override val generatorFactory = new TestCpgGeneratorFactory(config)
  }

  override def importCode = new MyImportCode(this)

}

class TestCpgGeneratorFactory(config: ConsoleConfig) extends CpgGeneratorFactory(config) {
  override def forCodeAt(
      inputPath: String,
  ): Option[CpgGenerator] = {
    Some(new FuzzyCTestingFrontend)
  }

  override def forLanguage(language: String): Option[CpgGenerator] = {
    Some(new FuzzyCTestingFrontend)
  }

  private class FuzzyCTestingFrontend extends CpgGenerator {

    override def generate(inputPath: String, outputPath: String, namespaces: List[String]): Option[String] = {
      val fuzzyc = new FuzzyC2Cpg()
      val cpg = fuzzyc.runAndOutput(Set(inputPath), Set(".c"), Some(outputPath))
      cpg.close()
      Some(outputPath)
    }

    def isAvailable: Boolean = true

  }

}
