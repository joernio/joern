package io.joern.console.testing

import better.files.Dsl.mkdir
import better.files.File
import io.joern.c2cpg.{C2Cpg, Config}
import io.joern.console.cpgcreation.{CpgGenerator, CpgGeneratorFactory, ImportCode}
import io.joern.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, DefaultAmmoniteExecutor, FrontendConfig, InstallConfig}
import io.shiftleft.codepropertygraph.generated.Languages

import java.nio.file.Path
import scala.util.Try

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
        Try(console.cpgs.foreach(cpg => cpg.close()))
        Try(console.workspace.reset())
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

  override def importCode: ImportCode[Project] = new ImportCode(this) {
    override val generatorFactory = new TestCpgGeneratorFactory(config)

    override def c: SourceBasedFrontend = new SourceBasedFrontend("testCFrontend", language = Languages.NEWC) {
      override def cpgGeneratorForLanguage(
        language: String,
        config: FrontendConfig,
        rootPath: Path,
        args: List[String]
      ): Option[CpgGenerator] = {
        val newConfig = new ConsoleConfig(TestConsole.this.config.install, config.withArgs(args))
        new TestCpgGeneratorFactory(newConfig).forLanguage(language)
      }
    }
  }
}

class TestCpgGeneratorFactory(config: ConsoleConfig) extends CpgGeneratorFactory(config) {
  override def forCodeAt(inputPath: String): Option[CpgGenerator] = {
    Some(new CTestingFrontend)
  }

  override def forLanguage(language: String): Option[CpgGenerator] = language match {
    case Languages.C | Languages.NEWC => Some(new CTestingFrontend)
    case _                            => None // no other languages are tested here
  }

  private class CTestingFrontend extends CpgGenerator {

    override def generate(inputPath: String, outputPath: String, namespaces: List[String]): Option[String] = {
      val c2cpg = new C2Cpg()
      val defines = config.frontend.cmdLineParams.toList
        .sliding(2)
        .toList
        .collect {
          case List(h, t) if h == "--define" => t
        }
      c2cpg.run(Config(inputPath, outputPath, defines = defines.toSet))
      Some(outputPath)
    }

    def isAvailable: Boolean = true

  }

}
