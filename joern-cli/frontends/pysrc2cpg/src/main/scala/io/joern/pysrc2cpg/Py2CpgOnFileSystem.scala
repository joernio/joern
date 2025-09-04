package io.joern.pysrc2cpg

import io.joern.x2cpg.passes.frontend.TypeRecoveryParserConfig
import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.*
import scala.util.Try
import scala.jdk.CollectionConverters.*

case class Py2CpgOnFileSystemConfig(
  venvDir: Option[Path] = None,
  venvDirs: Seq[Path] = Nil,
  ignoreVenvDir: Boolean = true,
  ignorePaths: Seq[Path] = Nil,
  ignoreDirNames: Seq[String] = Nil,
  requirementsTxt: String = "requirements.txt",
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig(),
  override val typeRecoveryParserConfig: TypeRecoveryParserConfig.Config = TypeRecoveryParserConfig.Config()
) extends X2CpgConfig[Py2CpgOnFileSystemConfig]
    with TypeRecoveryParserConfig {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Py2CpgOnFileSystemConfig =
    copy(genericConfig = value)

  override def withTypeRecoveryParserConfig(value: TypeRecoveryParserConfig.Config): Py2CpgOnFileSystemConfig =
    copy(typeRecoveryParserConfig = value)

  def withVenvDir(venvDir: Path): Py2CpgOnFileSystemConfig = {
    copy(venvDir = Option(venvDir))
  }

  def withVenvDirs(venvDirs: Seq[Path]): Py2CpgOnFileSystemConfig = {
    copy(venvDirs = venvDirs)
  }

  def withIgnoreVenvDir(value: Boolean): Py2CpgOnFileSystemConfig = {
    copy(ignoreVenvDir = value)
  }

  def withIgnorePaths(value: Seq[Path]): Py2CpgOnFileSystemConfig = {
    copy(ignorePaths = value)
  }

  def withIgnoreDirNames(value: Seq[String]): Py2CpgOnFileSystemConfig = {
    copy(ignoreDirNames = value)
  }

  def withRequirementsTxt(text: String): Py2CpgOnFileSystemConfig = {
    copy(requirementsTxt = text)
  }
}

class Py2CpgOnFileSystem extends X2CpgFrontend {
  override type ConfigType = Py2CpgOnFileSystemConfig
  override val defaultConfig: Py2CpgOnFileSystemConfig = Py2CpgOnFileSystemConfig()

  private val logger = LoggerFactory.getLogger(classOf[Py2CpgOnFileSystem])

  /** Entry point for files system based cpg generation from python code.
    * @param config
    *   Configuration for cpg generation.
    */
  override def createCpg(config: Py2CpgOnFileSystemConfig): Try[Cpg] = {
    logConfiguration(config)

    X2Cpg.withNewEmptyCpg(config.outputPath, config) { (cpg, _) =>
      val venvIgnorePath =
        if (config.ignoreVenvDir) {
          config.venvDir.toList ++ config.venvDirs
        } else {
          Nil
        }
      val inputPath           = Path.of(config.inputPath)
      val ignoreDirNamesSet   = config.ignoreDirNames.toSet
      val absoluteIgnorePaths = (config.ignorePaths ++ venvIgnorePath).map(inputPath.resolve)

      val inputFiles = SourceFiles
        .determine(
          config.inputPath,
          Set(".py"),
          ignoredFilesRegex = Option(config.ignoredFilesRegex),
          ignoredFilesPath = Option(config.ignoredFiles)
        )
        .map(x => Path.of(x))
        .filterNot { file =>
          isAutoDetectedVenv(config, file, inputPath) ||
          isIgnoredDir(file, inputPath, ignoreDirNamesSet) ||
          isInIgnoredAbsolutePaths(file, absoluteIgnorePaths)
        }

      val inputProviders = inputFiles.map { inputFile => () =>
        {
          val content = IOUtils.readEntireFile(inputFile)
          Py2Cpg.InputPair(content, inputPath.relativize(inputFile).toString)
        }
      }
      val py2Cpg = new Py2Cpg(inputProviders, cpg, config)
      py2Cpg.buildCpg()
    }
  }

  private def isInIgnoredAbsolutePaths(file: Path, absoluteIgnorePaths: Seq[Path]): Boolean = {
    absoluteIgnorePaths.exists(ignorePath => file.startsWith(ignorePath))
  }

  private def elementsOfPath(path: Path): List[Path] = {
    val elements = path.iterator().asScala.toList
    if (!Files.isDirectory(path)) {
      // we're only interested in the directories - drop the file part
      elements.dropRight(1)
    } else {
      elements
    }
  }

  private def isAutoDetectedVenv(config: Py2CpgOnFileSystemConfig, file: Path, inputPath: Path): Boolean = {
    if (!config.ignoreVenvDir || config.venvDirs.nonEmpty || config.venvDir.isDefined) {
      false
    } else {
      elementsOfPath(inputPath.relativize(file)).exists(inputPath.resolve(_).resolve("pyvenv.cfg").toFile.exists())
    }
  }

  private def isIgnoredDir(file: Path, inputPath: Path, ignoreDirNamesSet: Set[String]): Boolean = {
    elementsOfPath(inputPath.relativize(file)).exists(dir => ignoreDirNamesSet.contains(dir.toString))
  }

  private def logConfiguration(config: Py2CpgOnFileSystemConfig): Unit = {
    logger.info(s"Output file: ${config.outputPath}")
    logger.info(s"Input directory: ${config.inputPath}")
    logger.info(s"Venv directories: ${(config.venvDir.toList ++ config.venvDirs).mkString(", ")}")
    logger.info(s"IgnoreVenvDir: ${config.ignoreVenvDir}")
    logger.info(s"IgnorePaths: ${config.ignorePaths.mkString(", ")}")
    logger.info(s"IgnoreDirNames: ${config.ignoreDirNames.mkString(", ")}")
    logger.info(s"No dummy types: ${config.disableDummyTypes}")
    logger.info(s"Enable file content: ${!config.disableFileContent}")
    logger.info(s"Version: ${this.getClass.getPackage.getImplementationVersion}")
  }
}
