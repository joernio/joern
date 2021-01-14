package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import overflowdb.Graph

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.mutable

case class Py2CpgConfig(outputFile: String,
                        inputDir: String)

class Py2Cpg(config: Py2CpgConfig) {
  def buildCpg(): Unit = {
    val cpg = initCpg(config.outputFile)
    val keyPool = new IntervalKeyPool(1, Long.MaxValue)

    val inputFiles = collectInputFiles(config.inputDir)

    val astPass = new AstPass(cpg, inputFiles, keyPool)
    astPass.createAndApply()
  }

  private def initCpg(outputFileStr: String): Cpg = {
    val outputFile = Paths.get(outputFileStr)
    if (Files.exists(outputFile)) {
      Files.delete(outputFile)
    }

    val odbConfig = overflowdb.Config.withDefaults.withStorageLocation(outputFileStr)
    val graph = Graph.open(odbConfig,
      io.shiftleft.codepropertygraph.generated.nodes.Factories.allAsJava,
      io.shiftleft.codepropertygraph.generated.edges.Factories.allAsJava)
    new Cpg(graph)
  }

  private def collectInputFiles(inputDirStr: String): Iterable[String] = {
    val inputDir = Paths.get(inputDirStr)

    if (!Files.exists(inputDir)) {
      // TODO log
    }

    val inputFiles = mutable.ArrayBuffer.empty[String]

    Files.walkFileTree(inputDir, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val fileStr = file.toString
        if (fileStr.endsWith(".py")) {
          inputFiles.append(fileStr)
        }
        FileVisitResult.CONTINUE
      }
    })

    inputFiles
  }
}
