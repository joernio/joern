package io.joern.joerncli.console

import io.joern.console.defaultAvailableWidthProvider
import io.joern.console.workspacehandling.{ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, InstallConfig}
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg

import java.nio.file.Path

object JoernWorkspaceLoader {}

class JoernWorkspaceLoader extends WorkspaceLoader[JoernProject] {
  override def createProject(projectFile: ProjectFile, path: Path): JoernProject = {
    val project = new JoernProject(projectFile, path)
    project.context = EngineContext()
    project
  }
}

class JoernConsole extends Console[JoernProject](new JoernWorkspaceLoader) {

  override val config: ConsoleConfig = JoernConsole.defaultConfig

  implicit var semantics: Semantics = context.semantics

  // this is set to be `opts.ossdataflow` on initialization of the shell
  var ossDataFlowOptions: OssDataFlowOptions = new OssDataFlowOptions()

  implicit def context: EngineContext =
    workspace.getActiveProject
      .map(x => x.asInstanceOf[JoernProject].context)
      .getOrElse(EngineContext())

  def loadCpg(inputPath: String): Option[Cpg] = {
    report("Deprecated. Please use `importCpg` instead")
    importCpg(inputPath)
  }

  override def applyDefaultOverlays(cpg: Cpg): Cpg = {
    super.applyDefaultOverlays(cpg)
    _runAnalyzer(new OssDataFlow(ossDataFlowOptions))
  }

}

object JoernConsole {

  def banner(): String =
    s"""
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
        |Version: $version
        |$helpMsg
      """.stripMargin

  def version: String =
    getClass.getPackage.getImplementationVersion

  private def helpMsg: String =
    s"""Type `help` to begin""".stripMargin

  def defaultConfig: ConsoleConfig = new ConsoleConfig()

}
