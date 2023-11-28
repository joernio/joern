package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.TypeRecoveryParserConfig
import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.*
import scala.util.Try
import scala.jdk.CollectionConverters._

case class Py2CpgOnFileSystemConfig(
  venvDir: Path = Paths.get(".venv"),
  ignoreVenvDir: Boolean = true,
  ignorePaths: Seq[Path] = Nil,
  ignoreDirNames: Seq[String] = Nil,
  requirementsTxt: String = "requirements.txt"
) extends X2CpgConfig[Py2CpgOnFileSystemConfig]
    with TypeRecoveryParserConfig[Py2CpgOnFileSystemConfig] {
  def withVenvDir(venvDir: Path): Py2CpgOnFileSystemConfig = {
    copy(venvDir = venvDir).withInheritedFields(this)
  }

  def withIgnoreVenvDir(value: Boolean): Py2CpgOnFileSystemConfig = {
    copy(ignoreVenvDir = value).withInheritedFields(this)
  }

  def withIgnorePaths(value: Seq[Path]): Py2CpgOnFileSystemConfig = {
    copy(ignorePaths = value).withInheritedFields(this)
  }

  def withIgnoreDirNames(value: Seq[String]): Py2CpgOnFileSystemConfig = {
    copy(ignoreDirNames = value).withInheritedFields(this)
  }

  def withRequirementsTxt(text: String): Py2CpgOnFileSystemConfig = {
    copy(requirementsTxt = text).withInheritedFields(this)
  }
}

class Py2CpgOnFileSystem extends X2CpgFrontend[Py2CpgOnFileSystemConfig] {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Entry point for files system based cpg generation from python code.
    * @param config
    *   Configuration for cpg generation.
    */
  override def createCpg(config: Py2CpgOnFileSystemConfig): Try[Cpg] = {
    logConfiguration(config)

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, _) =>
      val venvIgnorePath =
        if (config.ignoreVenvDir) {
          config.venvDir :: Nil
        } else {
          Nil
        }
      val inputPath         = Path.of(config.inputPath)
      val ignoreDirNamesSet = config.ignoreDirNames.toSet
      val absoluteIgnorePaths = (config.ignorePaths ++ venvIgnorePath).map { path =>
        inputPath.resolve(path)
      }

      val inputFiles = SourceFiles
        .determine(
          config.inputPath,
          Set(".py"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        .map(x => Path.of(x))
        .filter { file => filterIgnoreDirNames(file, inputPath, ignoreDirNamesSet) }
        .filter { file =>
          !absoluteIgnorePaths.exists(ignorePath => file.startsWith(ignorePath))
        }

      val inputProviders = inputFiles.map { inputFile => () =>
        {
          val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
          Py2Cpg.InputPair(content, inputPath.relativize(inputFile).toString)
        }
      }
      val py2Cpg = new Py2Cpg(inputProviders, cpg, config.inputPath, config.requirementsTxt, config.schemaValidation)
      py2Cpg.buildCpg()
    }
  }

  private def filterIgnoreDirNames(file: Path, inputPath: Path, ignoreDirNamesSet: Set[String]): Boolean = {
    var parts = inputPath.relativize(file).iterator().asScala.toList

    if (!Files.isDirectory(file)) {
      // we're only interested in the directories - drop the file part
      parts = parts.dropRight(1)
    }

    val aPartIsInIgnoreSet = parts.exists(part => ignoreDirNamesSet.contains(part.toString))
    !aPartIsInIgnoreSet
  }

  private def logConfiguration(config: Py2CpgOnFileSystemConfig): Unit = {
    logger.info(s"Output file: ${config.outputPath}")
    logger.info(s"Input directory: ${config.inputPath}")
    logger.info(s"Venv directory: ${config.venvDir}")
    logger.info(s"IgnoreVenvDir: ${config.ignoreVenvDir}")
    logger.info(s"IgnorePaths: ${config.ignorePaths.mkString(", ")}")
    logger.info(s"IgnoreDirNames: ${config.ignoreDirNames.mkString(", ")}")
    logger.info(s"No dummy types: ${config.disableDummyTypes}")
  }
}
