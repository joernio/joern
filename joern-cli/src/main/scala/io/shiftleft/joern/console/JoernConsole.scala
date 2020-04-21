package io.shiftleft.joern.console

import java.nio.file.Path

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.{Console, ConsoleConfig, InstallConfig}
import io.shiftleft.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.shiftleft.joern.CpgLoader
import io.shiftleft.semanticcpg.layers.LayerCreator

class JoernWorkspaceLoader extends WorkspaceLoader {
  override def createProject(projectFile: ProjectFile, path: Path): Project = {
    Project(projectFile, path)
  }
}

class JoernConsole extends Console(JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

  override def config: ConsoleConfig = JoernConsole.config

  def banner(): Unit = {
    println("""
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
      """.stripMargin)
  }

  // If you remove this, the shell will not start.
  def version(): String = {
    // TODO read and report version
    ""
  }

  /**
    * (Re)-apply semantics stored in `semanticsFilenameOpt`.
    * If `semanticsFilenameOpt` is None default semantics
    * are applied.
    * */
  def applySemantics(semanticsFilenameOpt: Option[String]): Unit =
    CpgLoader.applySemantics(cpg, semanticsFilenameOpt)

  override def _runAnalyzer(overlayCreators: LayerCreator*): Cpg = {
    cpg
  }

  override def applyDefaultOverlays(cpg: Cpg): Unit = {}
}

object JoernConsole {

  def config: ConsoleConfig = new ConsoleConfig()

  def runScriptTest(scriptName: String, params: Map[String, String], cpg: Cpg): Any = {
    class TempConsole(workspaceDir: String) extends JoernConsole {
      override def config = new ConsoleConfig(
        install = new InstallConfig(Map("SHIFTLEFT_CONSOLE_INSTALL_DIR" -> workspaceDir))
      )
    }
    val workspaceDir = File.newTemporaryDirectory("console")
    try {
      new TempConsole(workspaceDir.toString).runScript(scriptName, params, cpg)
    } finally {
      workspaceDir.delete()
    }
  }

}
