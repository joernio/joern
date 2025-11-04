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

  override def runOnPart(diffGraph: DiffGraphBuilder, file: Path): Unit = {
    val contentMaybe = if (file.extension().contains(PlistExt)) {
      Try(PropertyListParser.parse(file.toFile).toXMLPropertyList)
    } else {
      Try(IOUtils.readEntireFile(file))
    }
    contentMaybe match {
      case Success(content) =>
        val configFileContent =
          s"""<!--This has been generated from ${file.toAbsolutePath}-->
             |$content""".stripMargin
        val name       = configFileName(file)
        val configNode = NewConfigFile().name(name).content(configFileContent)
        logger.debug(s"Adding config file $name")
        diffGraph.addNode(configNode)
      case Failure(error) =>
        logger.warn(s"Unable to create config file node for ${file.toAbsolutePath}: $error")
    }
  }
}
