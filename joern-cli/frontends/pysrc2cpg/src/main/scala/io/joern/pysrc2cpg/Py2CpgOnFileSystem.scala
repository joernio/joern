package io.joern.pysrc2cpg

import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import overflowdb.Graph

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

case class Py2CpgOnFileSystemConfig(outputFile: Path, inputDir: Path, ignoreVenvDir: Option[Path])

object Py2CpgOnFileSystem {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Entry point for files system based cpg generation from python code.
    * @param config
    *   Configuration for cpg generation.
    */
  def buildCpg(config: Py2CpgOnFileSystemConfig): Unit = {
    logConfiguration(config)

    val cpg = initCpg(config.outputFile)

    val inputFiles = collectInputFiles(config.inputDir, config.ignoreVenvDir.to(Iterable))
    val inputProviders = inputFiles.map { inputFile => () =>
      {
        val content = IOUtils.readLinesInFile(inputFile).mkString("\n")
        Py2Cpg.InputPair(content, inputFile.toString, config.inputDir.relativize(inputFile).toString)
      }
    }

    val py2Cpg = new Py2Cpg(inputProviders, cpg)
    py2Cpg.buildCpg()
    cpg.close
  }

  private def initCpg(outputFile: Path): Cpg = {
    if (Files.exists(outputFile)) {
      Files.delete(outputFile)
    }

    X2Cpg.newEmptyCpg(Some(outputFile.toString))
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
    config.ignoreVenvDir.foreach(dir => logger.info(s"Ignored virtual environment directory: $dir"))
  }
}
