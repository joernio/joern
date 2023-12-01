package io.joern.console.testing

import better.files.Dsl.mkdir
import better.files.File
import io.joern.console.cpgcreation.{CCpgGenerator, CpgGenerator, CpgGeneratorFactory, ImportCode}
import io.joern.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, FrontendConfig, InstallConfig}
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.utils.ProjectRoot

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
        Try(console.workspace.reset)
      }
    }
  }

}

object TestWorkspaceLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = Project(projectFile, path)
}

class TestConsole(workspaceDir: String) extends Console[Project](TestWorkspaceLoader, File(workspaceDir)) {
  override def config =
    new ConsoleConfig(install = new InstallConfig(Map("SHIFTLEFT_OCULAR_INSTALL_DIR" -> workspaceDir)))

  override def importCode: ImportCode[Project] = new ImportCode(this) {
    override val generatorFactory = new TestCpgGeneratorFactory(config)

    override def c: SourceBasedFrontend = new CFrontend("testCFrontend") {
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
  private def newCCpgGenerator() = {
    CCpgGenerator(
      config.frontend,
      Path.of(ProjectRoot.relativise("joern-cli/frontends/c2cpg/target/universal/stage/bin"))
    )

  }

  override def forCodeAt(inputPath: String): Option[CpgGenerator] = {
    Some(newCCpgGenerator())
  }

  override def forLanguage(language: String): Option[CpgGenerator] = language match {
    case Languages.C | Languages.NEWC => Some(newCCpgGenerator())
    case _                            => None // no other languages are tested here
  }

}
