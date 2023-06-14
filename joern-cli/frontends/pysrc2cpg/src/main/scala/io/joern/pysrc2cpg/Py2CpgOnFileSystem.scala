package io.joern.pysrc2cpg

import io.joern.x2cpg.{SourceFiles, X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file._
import scala.util.Try

case class Py2CpgOnFileSystemConfig(
  venvDir: Path = Paths.get(".venv"),
  ignoreVenvDir: Boolean = true,
  disableDummyTypes: Boolean = false,
  requirementsTxt: String = "requirements.txt"
) extends X2CpgConfig[Py2CpgOnFileSystemConfig] {
  def withVenvDir(venvDir: Path): Py2CpgOnFileSystemConfig = {
    copy(venvDir = venvDir).withInheritedFields(this)
  }

  def withIgnoreVenvDir(value: Boolean): Py2CpgOnFileSystemConfig = {
    copy(ignoreVenvDir = value).withInheritedFields(this)
  }

  def withDisableDummyTypes(value: Boolean): Py2CpgOnFileSystemConfig = {
    copy(disableDummyTypes = value).withInheritedFields(this)
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
      val ignorePrefixes =
        if (config.ignoreVenvDir) {
          config.venvDir :: Nil
        } else {
          Nil
        }
      val inputFiles = SourceFiles
        .determine(config.inputPath, Set(".py"), config)
        .map(x => Path.of(x))
        .filterNot { file =>
          val relativeFile = Path.of(config.inputPath).relativize(file)
          ignorePrefixes.exists(prefix => relativeFile.startsWith(prefix))
        }
      val inputProviders = inputFiles.map { inputFile => () =>
        {
          val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
          Py2Cpg.InputPair(content, inputFile.toString, Paths.get(config.inputPath).relativize(inputFile).toString)
        }
      }
      val py2Cpg = new Py2Cpg(inputProviders, cpg, config.inputPath, config.requirementsTxt)
      py2Cpg.buildCpg()
    }
  }

  private def logConfiguration(config: Py2CpgOnFileSystemConfig): Unit = {
    logger.info(s"Output file: ${config.outputPath}")
    logger.info(s"Input directory: ${config.inputPath}")
    logger.info(s"Venv directory: ${config.venvDir}")
    logger.info(s"IgnoreVenvDir: ${config.ignoreVenvDir}")
    logger.info(s"No dummy types: ${config.disableDummyTypes}")
  }
}
