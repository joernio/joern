package io.joern.console.testing

import io.joern.console.cpgcreation.{CpgGenerator, CpgGeneratorFactory, ImportCode}
import io.joern.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, FrontendConfig, InstallConfig}
import io.joern.console.cpgcreation.guessLanguage
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil

import java.nio.file.{Files, Path, Paths}
import scala.util.Try

object ConsoleFixture {
  def apply[T <: Console[Project]](constructor: String => T = { x =>
    new TestConsole(x)
  })(fun: (T, Path) => Unit): Unit = {
    FileUtil.usingTemporaryDirectory("console") { workspaceDir =>
      FileUtil.usingTemporaryDirectory("console") { codeDir =>
        Files.createDirectory(codeDir / "dir1")
        Files.createDirectory(codeDir / "dir2")

        val fooPath    = (codeDir / "dir1" / "foo.c")
        val fooContent = "int main(int argc, char **argv) { char *ptr = 0x1 + argv; return argc; }"

        val barPath    = (codeDir / "dir2" / "bar.c")
        val barContent = "int bar(int x) { return x; }"

        Files.writeString(fooPath, fooContent)
        Files.writeString(barPath, barContent)

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

class TestConsole(workspaceDir: String) extends Console[Project](TestWorkspaceLoader, Paths.get(workspaceDir)) {
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

    override def jssrc: SourceBasedFrontend =
      new JsFrontend("testJsSrcFrontend", Languages.JSSRC, "", "js") {
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

    override def java: SourceBasedFrontend =
      new SourceBasedFrontend("testJavaSrcFrontend", Languages.JAVASRC, "", "java") {
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

    override def swiftsrc: SourceBasedFrontend =
      new SwiftSrcFrontend("testSwiftSrcFrontend", Languages.SWIFTSRC, "", "swift") {
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
    guessLanguage(inputPath) match {
      case Some(Languages.NEWC)     => Option(InProcessCpgGenerator.c(config.frontend))
      case Some(Languages.JSSRC)    => Option(InProcessCpgGenerator.jssrc(config.frontend))
      case Some(Languages.SWIFTSRC) => Option(InProcessCpgGenerator.swiftsrc(config.frontend))
      case _                        => None // no other languages are tested here
    }
  }

  override def forLanguage(language: String): Option[CpgGenerator] = language match {
    case Languages.NEWC     => Option(InProcessCpgGenerator.c(config.frontend))
    case Languages.JSSRC    => Option(InProcessCpgGenerator.jssrc(config.frontend))
    case Languages.SWIFTSRC => Option(InProcessCpgGenerator.swiftsrc(config.frontend))
    case _                  => None // no other languages are tested here
  }

}
