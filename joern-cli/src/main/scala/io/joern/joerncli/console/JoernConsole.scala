package io.joern.joerncli.console

import better.files.Dsl._
import better.files._
import io.joern.console.workspacehandling.{ProjectFile, WorkspaceLoader}
import io.joern.console.{Console, ConsoleConfig, InstallConfig}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.Cpg

import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.jdk.CollectionConverters._

object JoernWorkspaceLoader {
  val semanticsFilename = "semantics"

  lazy val defaultSemanticsFile: String = {
    val file        = Files.createTempFile("joern-default", ".semantics")
    val defaultFile = this.getClass.getClassLoader.getResource("default.semantics").toURI

    // Weird, yes, but necessary when running as a distribution.
    // See (https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html)
    if (defaultFile.getScheme.contains("jar")) {
      FileSystems.newFileSystem(defaultFile, Map("create" -> "false").asJava)
    }
    val fileLines = Files.readAllLines(Paths.get(defaultFile))
    Files.write(file, fileLines, java.nio.charset.StandardCharsets.UTF_8).toString
  }

  def defaultSemantics: Semantics = Semantics.fromList(new Parser().parseFile(defaultSemanticsFile))

}

class JoernWorkspaceLoader extends WorkspaceLoader[JoernProject] {
  override def createProject(projectFile: ProjectFile, path: Path): JoernProject = {
    val project               = new JoernProject(projectFile, path)
    val semanticFileInProject = path.resolve(JoernWorkspaceLoader.semanticsFilename)
    cp(File(JoernWorkspaceLoader.defaultSemanticsFile), File(semanticFileInProject))
    project.context = EngineContext(JoernWorkspaceLoader.defaultSemantics)
    project
  }
}

class JoernConsole extends Console[JoernProject](JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

  override val config: ConsoleConfig = JoernConsole.defaultConfig

  implicit def semantics: Semantics = context.semantics

  implicit def context: EngineContext =
    workspace.getActiveProject
      .map(x => x.asInstanceOf[JoernProject].context)
      .getOrElse(EngineContext(JoernWorkspaceLoader.defaultSemantics))

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
      override def context: EngineContext =
        EngineContext(Semantics.fromList(new Parser().parseFile(JoernWorkspaceLoader.defaultSemanticsFile)))
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
