package io.joern.pysrc2cpg

import io.joern.x2cpg.{X2Cpg, X2CpgConfig, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable
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
      val inputFiles = collectInputFiles(Paths.get(config.inputPath), ignorePrefixes, config.requirementsTxt)
      val inputProviders = inputFiles._1.map { inputFile => () =>
        {
          val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
          Py2Cpg.InputPair(content, inputFile.toString, Paths.get(config.inputPath).relativize(inputFile).toString)
        }
      }
      val configInputProviders =
        inputFiles._2.map { inputFile => () =>
          {
            val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
            Py2Cpg.InputPair(content, inputFile.toString, Paths.get(config.inputPath).relativize(inputFile).toString)
          }
        }
      val py2Cpg = new Py2Cpg(inputProviders, configInputProviders, cpg)
      py2Cpg.buildCpg()
    }
  }

  private def collectInputFiles(
    inputDir: Path,
    ignorePrefixes: Iterable[Path],
    requirementsTxtFileName: String
  ): (Iterable[Path], Option[Path]) = {
    if (!Files.exists(inputDir)) {
      logger.error(s"Cannot find $inputDir")
      return (Iterable.empty, None)
    }

    val inputFiles = mutable.ArrayBuffer.empty[Path]

    Files.walkFileTree(
      inputDir,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val relativeFile    = inputDir.relativize(file)
          val relativeFileStr = relativeFile.toString
          if (
            relativeFileStr.endsWith(".py") &&
            !ignorePrefixes.exists(prefix => relativeFile.startsWith(prefix))
          ) {
            inputFiles.append(file)
          }
          FileVisitResult.CONTINUE
        }
      }
    )

    var requirementsTxtMaybe: Option[Path] = None
    Files.walkFileTree(
      inputDir,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val relativeFile    = inputDir.relativize(file)
          val relativeFileStr = relativeFile.toString
          if (relativeFileStr.endsWith(requirementsTxtFileName)) {
            requirementsTxtMaybe = Some(file)
          }
          FileVisitResult.CONTINUE
        }
      }
    )
    (inputFiles, requirementsTxtMaybe)
  }

  private def logConfiguration(config: Py2CpgOnFileSystemConfig): Unit = {
    logger.info(s"Output file: ${config.outputPath}")
    logger.info(s"Input directory: ${config.inputPath}")
    logger.info(s"Venv directory: ${config.venvDir}")
    logger.info(s"IgnoreVenvDir: ${config.ignoreVenvDir}")
    logger.info(s"No dummy types: ${config.disableDummyTypes}")
  }
}
