package io.joern.javasrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

class ConfigFileCreationPass(projectDir: String, cpg: Cpg) extends ConcurrentWriterCpgPass[File](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[File] = {
    Try(File(projectDir)) match {
      case Success(file) if file.isDirectory =>
        file.listRecursively
          .filter(isConfigFile)
          .toArray

      case Success(file) if isConfigFile(file) =>
        Array(file)

      case _ => Array.empty
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, file: File): Unit = {
    Try(IOUtils.readLinesInFile(file.path)) match {
      case Success(fileContentsLines) =>
        val name       = configFileName(file)
        val content    = fileContentsLines.mkString("\n")
        val configNode = NewConfigFile().name(name).content(content)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)

      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.canonicalPath}: $error")
    }
  }

  private def configFileName(configFile: File): String = {
    Try(Paths.get(projectDir)) match {
      case Success(basePath) =>
        basePath.relativize(configFile.path).toString

      case Failure(_) =>
        configFile.name
    }
  }

  private def extensionFilter(extension: String)(file: File): Boolean = {
    file.extension.contains(extension)
  }

  private def pathEndFilter(pathEnd: String)(file: File): Boolean = {
    file.canonicalPath.endsWith(pathEnd)
  }

  private def mybatisFilter(file: File): Boolean = {
    file.canonicalPath.contains("batis") && file.extension.contains(".xml")
  }

  private val configFileFilters: List[File => Boolean] = List(
    // JAVA_INTERNAL
    extensionFilter(".properties"),
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
    mybatisFilter
  )

  private def isConfigFile(file: File): Boolean = {
    configFileFilters.exists(predicate => predicate(file))
  }
}
