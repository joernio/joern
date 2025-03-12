package io.joern.x2cpg.passes.frontend

import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

/** Scans for and inserts configuration files into the CPG. Relies on the MetaData's `ROOT` property to provide the path
  * to scan, but alternatively one can specify a directory on the `rootDir` parameter.
  */
abstract class XConfigFileCreationPass(cpg: Cpg, private val rootDir: Option[String] = None)
    extends ForkJoinParallelCpgPass[Path](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  // File filters to override by the implementing class
  protected val configFileFilters: List[Path => Boolean]

  override def generateParts(): Array[Path] = {
    rootDir.orElse(cpg.metaData.root.headOption) match {
      case Some(root) =>
        Try(Paths.get(root)) match {
          case Success(file) if Files.isDirectory(file) =>
            file
              .walk()
              .filterNot(_ == file)
              .filter(isConfigFile)
              .toArray

          case Success(file) if isConfigFile(file) => Array(file)

          case _ => Array.empty
        }
      case None =>
        logger.warn("Unable to recover project directory for configuration file pass.")
        Array.empty
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, file: Path): Unit = {
    Try(IOUtils.readEntireFile(file)) match {
      case Success(content) =>
        val name       = configFileName(file)
        val configNode = NewConfigFile().name(name).content(content)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)

      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.toAbsolutePath}: $error")
    }
  }

  private def configFileName(configFile: Path): String = {
    Try(Paths.get(rootDir.getOrElse(cpg.metaData.root.head)).toAbsolutePath)
      .map(_.relativize(configFile.toAbsolutePath).toString)
      .getOrElse(configFile.fileName)
  }

  protected def extensionFilter(extension: String)(file: Path): Boolean = {
    file.extension().contains(extension)
  }

  protected def pathEndFilter(pathEnd: String)(file: Path): Boolean = {
    file.absolutePathAsString.endsWith(pathEnd)
  }

  private def isConfigFile(file: Path): Boolean = {
    configFileFilters.exists(predicate => predicate(file))
  }
}

/** Parses common Java related configuration files. Multiple frontends cover JVM languages so this is in a single shared
  * spot.
  */
class JavaConfigFileCreationPass(cpg: Cpg, rootDir: Option[String] = None)
    extends XConfigFileCreationPass(cpg, rootDir) {

  override val configFileFilters: List[Path => Boolean] = List(
    // JAVA_INTERNAL
    extensionFilter(".properties"),
    pathEndFilter("MANIFEST.MF"),
    // JSP
    extensionFilter(".jsp"),
    // Velocity files, see https://velocity.apache.org
    extensionFilter(".vm"),
    // For Terraform secrets
    extensionFilter(".tf"),
    extensionFilter(".tfvars"),
    // PLAY
    pathEndFilter("routes"),
    pathEndFilter("application.conf"),
    // SERVLET
    pathEndFilter("web.xml"),
    // JSF
    pathEndFilter("faces-config.xml"),
    // STRUTS
    pathEndFilter("struts.xml"),
    // DIRECT WEB REMOTING
    pathEndFilter("dwr.xml"),
    // MYBATIS
    mybatisFilter,
    // BUILD SYSTEM
    extensionFilter(".gradle"),
    pathEndFilter(".gradle.kts"),
    // ANDROID
    pathEndFilter("AndroidManifest.xml"),
    // SPRING
    extensionFilter(".yaml"),
    extensionFilter(".yml"),
    // JPA
    pathEndFilter("persistence.xml"),
    // HIBERNATE
    pathEndFilter("cfg.xml"),
    // MAVEN
    pathEndFilter("pom.xml")
  )

  private def mybatisFilter(file: Path): Boolean = {
    file.absolutePathAsString.contains("batis") && file.extension().contains(".xml")
  }

}
