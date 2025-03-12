package io.joern.x2cpg.utils

import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.nowarn
import scala.collection.mutable

trait TestCodeWriter {

  private val codeFileNamePairs             = mutable.ArrayBuffer.empty[(String, Option[String])]
  private var fileNameCounter               = 0
  private var outputDirectory: Option[Path] = None
  @nowarn
  protected def codeFilePreProcessing(codeFile: Path): Unit = {}

  @nowarn
  protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit = {}

  def moreCode(code: String): this.type = {
    codeFileNamePairs.append((code, None))
    this
  }

  def moreCode(code: String, fileName: String): this.type = {
    codeFileNamePairs.append((code, Option(fileName)))
    this
  }

  def writeCode(extension: String): Path = {
    if (outputDirectory.nonEmpty) {
      throw new RuntimeException("TestCodeWriter may only be used to write code once")
    }
    val tmpDir = Files.createTempDirectory("x2cpgTestTmpDir")
    FileUtil.deleteOnExit(tmpDir)
    outputDirectory = Some(tmpDir)

    val codeFiles = codeFileNamePairs.map { case (code, explicitFileName) =>
      val fileName = explicitFileName.getOrElse {
        val filename = s"Test$fileNameCounter$extension"
        fileNameCounter += 1
        filename
      }
      val filePath = Path.of(fileName)
      if (filePath.getParent != null) {
        Files.createDirectories(tmpDir.resolve(filePath.getParent))
      }
      val codeAsBytes = code.getBytes(StandardCharsets.UTF_8)
      val codeFile    = tmpDir.resolve(filePath)
      Files.write(codeFile, codeAsBytes)
      codeFilePreProcessing(codeFile)
      codeFile
    }.toList
    codeDirPreProcessing(tmpDir, codeFiles)
    tmpDir
  }

  def cleanupOutput(): Unit = {
    outputDirectory.foreach(FileUtil.delete(_))
  }
}
