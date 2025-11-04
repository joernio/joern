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

  private def isBinaryPlist(file: Path): Boolean = {
    if (!file.extension().contains(PlistExt)) return false
    Try(IOUtils.readLinesInFile(file)) match {
      case Success(dataBeginning :: _) => dataBeginning.trim.startsWith("bplist")
      case _                           => false
    }
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, file: Path): Unit = {
    val parseResult = if (isBinaryPlist(file)) {
      val header = s"<!--This has been generated from ${file.toAbsolutePath}-->\n"
      Try((PropertyListParser.parse(file.toFile).toXMLPropertyList, header))
    } else {
      Try((IOUtils.readEntireFile(file), ""))
    }
    parseResult match {
      case Success((content, header)) =>
        val configFileContent = s"$header$content"
        val name              = configFileName(file)
        val configNode        = NewConfigFile().name(name).content(configFileContent)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)
      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.toAbsolutePath}: $error")
    }
  }
}
