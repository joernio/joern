package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import org.slf4j.LoggerFactory
import overflowdb.Graph

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

case class Py2CpgConfig(outputFile: String, inputFileOrDir: String)

object Py2Cpg {
  private val logger = LoggerFactory.getLogger(getClass)
}

class Py2Cpg(config: Py2CpgConfig) {
  import Py2Cpg._

  def buildCpg(): Unit = {
    val cpg = initCpg(config.outputFile)
    val keyPool = new IntervalKeyPool(1, Long.MaxValue)

    val inputFiles = collectInputFiles(config.inputFileOrDir)

    val filesToCpgPass = new FilesToCpgPass(cpg, inputFiles, keyPool)
    filesToCpgPass.createAndApply()
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

  private def collectInputFiles(inputFileOrDir: String): Iterable[Path] = {
    val inputPath = Paths.get(inputFileOrDir)

    if (!Files.exists(inputPath)) {
      logger.error(s"Cannot find $inputFileOrDir")
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
