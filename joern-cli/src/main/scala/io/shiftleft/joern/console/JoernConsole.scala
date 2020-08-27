package io.shiftleft.joern.console

import java.nio.file.{FileSystems, Files, Path, Paths}

import better.files._
import better.files.Dsl._

import scala.jdk.CollectionConverters._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.console.{Console, ConsoleConfig, InstallConfig}
import io.shiftleft.console.workspacehandling.{Project, ProjectFile, WorkspaceLoader}
import io.shiftleft.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.dataflowengineoss.semanticsloader.{Parser, Semantics}

object JoernWorkspaceLoader {
  val semanticsFilename = "semantics"

  lazy val defaultSemanticsFile: String = {
    val file = Files.createTempFile("joern-default", ".semantics")
    val defaultFile = this.getClass.getClassLoader.getResource("default.semantics").toURI

    // Weird, yes, but necessary when running as a distribution.
    // See (https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html)
    if (defaultFile.getScheme.contains("jar")) {
      FileSystems.newFileSystem(defaultFile, Map("create" -> "false").asJava)
    }
    val fileLines = Files.readAllLines(Paths.get(defaultFile))
    Files.write(file, fileLines, java.nio.charset.StandardCharsets.UTF_8).toString
  }

  def defaultSemantics: Semantics = Semantics.fromList(
    new Parser().parseFile(defaultSemanticsFile)
  )

}

class JoernWorkspaceLoader extends WorkspaceLoader[JoernProject] {
  override def createProject(projectFile: ProjectFile, path: Path): JoernProject = {
    val project = new JoernProject(projectFile, path)
    val semanticFileInProject = path.resolve(JoernWorkspaceLoader.semanticsFilename)
    cp(File(JoernWorkspaceLoader.defaultSemanticsFile), File(semanticFileInProject))
    project.context = EngineContext(JoernWorkspaceLoader.defaultSemantics)
    project
  }
}

class JoernConsole extends Console[JoernProject](JoernAmmoniteExecutor, new JoernWorkspaceLoader) {

  override def config: ConsoleConfig = JoernConsole.config

  implicit def context: EngineContext =
    workspace.getActiveProject
      .map(x => EngineContext(x.asInstanceOf[JoernProject].context.semantics))
      .getOrElse(EngineContext(JoernWorkspaceLoader.defaultSemantics))

  def banner(): Unit = {
    println("""
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
      """.stripMargin)
    println(helpMsg())
  }

  private def helpMsg(): String =
    s"""Type `help` or `browse(help)` to begin""".stripMargin

  // If you remove this, the shell will not start.
  def version(): String = {
    // TODO read and report version
    ""
  }

  def loadCpg(inputPath: String): Option[Cpg] = {
    report("Deprecated. Please use `importCpg` instead")
    importCpg(inputPath)
  }

}

object JoernConsole {

  def config: ConsoleConfig = new ConsoleConfig()

  def runScriptTest(scriptName: String, params: Map[String, String], cpg: Cpg): Any = {
    class TempConsole(workspaceDir: String) extends JoernConsole {
      override def context: EngineContext =
        EngineContext(
          Semantics.fromList(
            new Parser().parseFile(JoernWorkspaceLoader.defaultSemanticsFile)
          ))
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
