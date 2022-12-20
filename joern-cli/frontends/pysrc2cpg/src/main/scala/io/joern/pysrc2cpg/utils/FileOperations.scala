package io.joern.pysrc2cpg.utils

import org.slf4j.LoggerFactory

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import scala.collection.mutable

object FileOperations {
  private val logger = LoggerFactory.getLogger(getClass)

  def collectInputFiles(inputDir: Path, ignorePrefixes: Iterable[Path]): Iterable[Path] = {
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
}
