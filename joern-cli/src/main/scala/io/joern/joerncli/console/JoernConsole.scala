package io.joern.joerncli.console

import better.files._
import io.joern.console.workspacehandling.{ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, InstallConfig}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.Cpg

import java.nio.file.Path

object JoernWorkspaceLoader {}

class JoernWorkspaceLoader extends WorkspaceLoader[JoernProject] {
  override def createProject(projectFile: ProjectFile, path: Path): JoernProject = {
    val project = new JoernProject(projectFile, path)
    project.context = EngineContext()
    project
  }
}

class JoernConsole extends Console[JoernProject](JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

  override val config: ConsoleConfig = JoernConsole.defaultConfig

  implicit var semantics: Semantics = context.semantics

  implicit def context: EngineContext =
    workspace.getActiveProject
      .map(x => x.asInstanceOf[JoernProject].context)
      .getOrElse(EngineContext())

  def banner(): Unit = {
    println(s"""
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
        |Version: $version
        |$helpMsg
      """.stripMargin)
  }

  private def helpMsg: String =
    s"""Type `help` or `browse(help)` to begin""".stripMargin

  def version: String =
    getClass.getPackage.getImplementationVersion

  def loadCpg(inputPath: String): Option[Cpg] = {
    report("Deprecated. Please use `importCpg` instead")
    importCpg(inputPath)
  }

}

object JoernConsole {

  def defaultConfig: ConsoleConfig = new ConsoleConfig()

  def runScriptTest(scriptName: String, params: Map[String, String], cpg: Cpg): Any = {
    class TempConsole(workspaceDir: String) extends JoernConsole {
      override def context: EngineContext = EngineContext()
      override val config = new ConsoleConfig(
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
