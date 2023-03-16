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
  outputFile: Path = Paths.get(X2CpgConfig.defaultOutputPath),
  inputDir: Path = null,
  venvDir: Path = Paths.get(".venv"),
  ignoreVenvDir: Boolean = true,
  disableDummyTypes: Boolean = false
) extends X2CpgConfig[Py2CpgOnFileSystemConfig] {
  override def withInputPath(inputPath: String): Py2CpgOnFileSystemConfig = {
    copy(inputDir = Paths.get(inputPath))
  }

  override def withOutputPath(x: String): Py2CpgOnFileSystemConfig = {
    copy(outputFile = Paths.get(x))
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

    X2Cpg.withNewEmptyCpg(config.outputFile.toString, config) { (cpg, _) =>
      val ignorePrefixes =
        if (config.ignoreVenvDir) {
          config.venvDir :: Nil
        } else {
          Nil
        }
      val inputFiles = collectInputFiles(config.inputDir, ignorePrefixes)
      val inputProviders = inputFiles.map { inputFile => () =>
        {
          val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
          Py2Cpg.InputPair(content, inputFile.toString, config.inputDir.relativize(inputFile).toString)
        }
      }

      val py2Cpg = new Py2Cpg(inputProviders, cpg)
      py2Cpg.buildCpg()
    }
  }

  private def collectInputFiles(inputDir: Path, ignorePrefixes: Iterable[Path]): Iterable[Path] = {
    if (!Files.exists(inputDir)) {
      logger.error(s"Cannot find $inputDir")
      return Iterable.empty
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

    inputFiles
  }

  private def logConfiguration(config: Py2CpgOnFileSystemConfig): Unit = {
    logger.info(s"Output file: ${config.outputFile}")
    logger.info(s"Input directory: ${config.inputDir}")
    logger.info(s"Venv directory: ${config.venvDir}")
    logger.info(s"IgnoreVenvDir: ${config.ignoreVenvDir}")
    logger.info(s"No dummy types: ${config.disableDummyTypes}")
  }
}
