package io.shiftleft.joern.console

import java.nio.file.Path

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.{Console, ConsoleConfig, InstallConfig}
import io.shiftleft.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.shiftleft.joern.CpgLoader
import io.shiftleft.joern.scripting.JoernAmmoniteExecutor
import io.shiftleft.semanticcpg.layers.LayerCreator

class JoernWorkspaceLoader extends WorkspaceLoader {
  override def createProject(projectFile: ProjectFile, path: Path): Project = {
    Project(projectFile, path)
  }
}

class JoernConsole extends Console(JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

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

  private var _cpg: Cpg = null

  /**
    * Return the CPG that was last loaded
    * */
  override def cpg: Cpg = _cpg match {
    case null       => throw new RuntimeException("No CPG loaded. Use `loadCpg(filename)` first")
    case value: Cpg => value
  }

  /**
    * Load CPG stored at `inputPath`.
    * */
  def loadCpg(inputPath: String): Option[Cpg] = {
    if (!File(inputPath).exists) {
      report(s"File does not exist: $inputPath")
      return None
    }
    _cpg = CpgLoader.loadFromOdb(inputPath)
    Some(cpg)
  }

  /**
    * (Re)-apply semantics stored in `semanticsFilenameOpt`.
    * If `semanticsFilenameOpt` is None default semantics
    * are applied.
    * */
  def applySemantics(semanticsFilenameOpt: Option[String]): Unit =
    CpgLoader.applySemantics(cpg, semanticsFilenameOpt)

  override def _runAnalyzer(overlayCreators: LayerCreator*): Cpg = ???

  private val _config = new ConsoleConfig()
  override def config: ConsoleConfig = _config

  override def applyDefaultOverlays(cpg: Cpg): Unit = ???
}

object JoernConsole {
  def runScript(scriptName: String, params: Map[String, String], cpg: Cpg): Any = {
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
