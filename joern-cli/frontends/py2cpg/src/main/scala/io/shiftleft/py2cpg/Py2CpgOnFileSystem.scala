package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory
import overflowdb.Graph

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

case class Py2CpgOnFileSystemConfig(outputFile: String, inputDir: String)

object Py2CpgOnFileSystem {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Entry point for files system based cpg generation from python code.
    * @param config Configuration for cpg generation.
    */
  def buildCpg(config: Py2CpgOnFileSystemConfig): Unit = {
    val cpg = initCpg(config.outputFile)

    val inputDirAsPath = Paths.get(config.inputDir)
    val inputFiles = collectInputFiles(config.inputDir)
    val inputProviders = inputFiles.map { inputFile => () =>
      {
        val content = Files.readAllBytes(inputFile)
        val contentStr = new String(content, StandardCharsets.UTF_8)
        Py2Cpg.InputPair(contentStr, inputDirAsPath.relativize(inputFile).toString)
      }
    }

    val py2Cpg = new Py2Cpg(inputProviders, cpg)
    py2Cpg.buildCpg()
    cpg.close
  }

  private def initCpg(outputFileStr: String): Cpg = {
    val outputFile = Paths.get(outputFileStr)
    if (Files.exists(outputFile)) {
      Files.delete(outputFile)
    }

    val odbConfig = overflowdb.Config.withDefaults.withStorageLocation(outputFileStr)
    val graph = Graph.open(
      odbConfig,
      io.shiftleft.codepropertygraph.generated.nodes.Factories.allAsJava,
      io.shiftleft.codepropertygraph.generated.edges.Factories.allAsJava
    )
    new Cpg(graph)
  }

  private def collectInputFiles(inputDir: String): Iterable[Path] = {
    val inputPath = Paths.get(inputDir)

    if (!Files.exists(inputPath)) {
      logger.error(s"Cannot find $inputDir")
      return Iterable.empty
    }

    val inputFiles = mutable.ArrayBuffer.empty[Path]

    Files.walkFileTree(
      inputPath,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val fileStr = file.toString
          if (fileStr.endsWith(".py")) {
            inputFiles.append(file)
          }
          FileVisitResult.CONTINUE
        }
      }
    )

    inputFiles
  }
}
