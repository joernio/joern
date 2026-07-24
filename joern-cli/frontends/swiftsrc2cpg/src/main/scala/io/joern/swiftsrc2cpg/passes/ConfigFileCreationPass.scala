package io.joern.swiftsrc2cpg.passes

import com.dd.plist.PropertyListParser
import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class ConfigFileCreationPass(cpg: Cpg, config: Config)
    extends XConfigFileCreationPass(
      cpg,
      config = config.withDefaultIgnoredFilesRegex(AstGenRunner.AstGenDefaultIgnoreRegex)
    ) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val PlistExt: String = ".plist"

  override val configFileFilters: List[Path => Boolean] = List(extensionFilter(PlistExt), extensionFilter(".xib"))

  /** Reads the file once and returns its content together with an optional source comment. Binary plists are converted
    * to XML via [[PropertyListParser]] and annotated with a comment recording their origin; text files are returned
    * as-is with an empty comment.
    */
  private def parseConfigFile(file: Path): Try[(String, String)] = Try {
    val content       = IOUtils.readEntireFile(file)
    val isBinaryPlist = file.extension().contains(PlistExt) && content.trim.startsWith("bplist")
    if (isBinaryPlist) {
      val sourceComment = s"\n<!--This file was generated from ${file.toAbsolutePath}-->"
      (PropertyListParser.parse(file.toFile).toXMLPropertyList, sourceComment)
    } else {
      (content, "")
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, file: Path): Unit = {
    parseConfigFile(file) match {
      case Success((content, sourceComment)) =>
        val configFileContent = s"$content$sourceComment"
        val name              = configFileName(file)
        val configNode        = NewConfigFile().name(name).content(configFileContent)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)
      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.toAbsolutePath}: $error")
    }
  }
}
