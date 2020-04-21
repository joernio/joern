package io.shiftleft.joern.console

import java.nio.file.Path

import better.files.File
import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.{Console, ConsoleConfig, InstallConfig}
import io.shiftleft.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.shiftleft.joern.CpgLoader
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, Scpg}

class JoernWorkspaceLoader extends WorkspaceLoader[Project] {
  override def createProject(projectFile: ProjectFile, path: Path): Project = {
    Project(projectFile, path)
  }
}

class JoernConsole extends Console[Project](JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

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

  override protected def runCreator(creator: LayerCreator, serializedCpg: SerializedCpg): Unit = {
    val context = new LayerCreatorContext(cpg, serializedCpg)
    creator.run(context, serializeInverse = true)
  }

  /**
    * (Re)-apply semantics stored in `semanticsFilenameOpt`.
    * If `semanticsFilenameOpt` is None default semantics
    * are applied.
    * */
  def applySemantics(semanticsFilenameOpt: Option[String]): Unit =
    CpgLoader.applySemantics(cpg, semanticsFilenameOpt)

  override def applyDefaultOverlays(cpg: Cpg): Unit = {
    val appliedOverlays = io.shiftleft.semanticcpg.Overlays.appliedOverlays(cpg)
    if (appliedOverlays.isEmpty && !(new Scpg().probe(cpg))) {
      report("Adding default overlays to base CPG")
      val overlayCreators = List(new Scpg)
      _runAnalyzer(overlayCreators :_*)
    }
  }

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
