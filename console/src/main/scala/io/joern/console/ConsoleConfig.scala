package io.joern.console

import better.files.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Installation configuration of Console
  *
  * @param environment
  *   A map of system environment variables.
  */
class InstallConfig(environment: Map[String, String] = sys.env) {

  /** determining the root path of the joern/ocular installation is rather complex unfortunately, because we support a
    * variety of use cases:
    *   - running the installed distribution from the install dir
    *   - running the installed distribution anywhere else on the system
    *   - running a locally staged ocular/joern build (via `sbt stage` and then either `./joern` or `cd
    *     joern-cli/target/universal/stage; ./joern`)
    *   - running a unit/integration test (note: the jars would be in the local cache, e.g. in ~/.coursier/cache)
    */
  lazy val rootPath: File = {
    if (environment.contains("SHIFTLEFT_OCULAR_INSTALL_DIR")) {
      environment("SHIFTLEFT_OCULAR_INSTALL_DIR").toFile
    } else {
      val uriToLibDir  = classOf[io.joern.console.InstallConfig].getProtectionDomain.getCodeSource.getLocation.toURI
      val pathToLibDir = File(uriToLibDir).parent
      findRootDirectory(pathToLibDir).getOrElse {
        val cwd = File.currentWorkingDirectory
        findRootDirectory(cwd).getOrElse(throw new AssertionError(s"""unable to find root installation directory
                                   | context: tried to find marker file `$rootDirectoryMarkerFilename`
                                   | started search in both $pathToLibDir and $cwd and searched 
                                   | $maxSearchDepth directories upwards""".stripMargin))
      }
    }
  }

  private val rootDirectoryMarkerFilename = ".installation_root"
  private val maxSearchDepth              = 10

  @tailrec
  private def findRootDirectory(currentSearchDir: File, currentSearchDepth: Int = 0): Option[File] = {
    if (currentSearchDir.list.map(_.name).contains(rootDirectoryMarkerFilename))
      Some(currentSearchDir)
    else if (currentSearchDepth < maxSearchDepth && currentSearchDir.parentOption.isDefined)
      findRootDirectory(currentSearchDir.parent)
    else
      None
  }
}

object InstallConfig {
  def apply(): InstallConfig = new InstallConfig()
}

class ConsoleConfig(
  val install: InstallConfig = InstallConfig(),
  val frontend: FrontendConfig = FrontendConfig(),
  val tools: ToolsConfig = ToolsConfig()
) {}

object ToolsConfig {

  private val osSpecificOpenCmd: String = {
    if (scala.util.Properties.isWin) "start"
    else if (scala.util.Properties.isMac) "open"
    else "xdg-open"
  }

  def apply(): ToolsConfig = new ToolsConfig()
}

class ToolsConfig(var imageViewer: String = ToolsConfig.osSpecificOpenCmd)

class FrontendConfig(var cmdLineParams: Iterable[String] = mutable.Buffer()) {
  def withArgs(args: Iterable[String]): FrontendConfig = {
    new FrontendConfig(cmdLineParams ++ args)
  }
}

object FrontendConfig {
  def apply(): FrontendConfig = new FrontendConfig()
}
