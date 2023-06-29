package io.joern.x2cpg.passes.frontend

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

/** Scans for and inserts configuration files into the CPG. Relies on the MetaData's `ROOT` property to provide the path
  * to scan.
  */
abstract class XConfigFileCreationPass(cpg: Cpg) extends ConcurrentWriterCpgPass[File](cpg) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  // File filters to override by the implementing class
  protected val configFileFilters: List[File => Boolean]

  private val rootDir = cpg.metaData.root.headOption.getOrElse("<empty>")

  override def generateParts(): Array[File] =
    if (rootDir.isBlank) {
      logger.warn("Unable to recover project directory for configuration file pass.")
      Array.empty
    } else {
      Try(File(rootDir)) match {
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
    Try(IOUtils.readEntireFile(file.path)) match {
      case Success(content) =>
        val name       = configFileName(file)
        val configNode = NewConfigFile().name(name).content(content)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)

      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.canonicalPath}: $error")
    }
  }

  private def configFileName(configFile: File): String = {
    Try(Paths.get(rootDir).toAbsolutePath)
      .map(_.relativize(configFile.path.toAbsolutePath).toString)
      .orElse(Try(configFile.pathAsString))
      .getOrElse(configFile.name)
  }

  protected def extensionFilter(extension: String)(file: File): Boolean = {
    file.extension.contains(extension)
  }

  protected def pathEndFilter(pathEnd: String)(file: File): Boolean = {
    file.canonicalPath.endsWith(pathEnd)
  }

  private def isConfigFile(file: File): Boolean = {
    configFileFilters.exists(predicate => predicate(file))
  }
}
