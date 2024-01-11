package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2CpgConfig
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.Graph

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.annotation.nowarn
import scala.collection.mutable

// Lazily populated test CPG which is created upon first access to the underlying graph.
// The trait LanguageFrontend is mixed in and not property/field of this class in order
// to allow the configuration of language frontend specific properties on the CPG object.
abstract class TestCpg extends Cpg() with LanguageFrontend {
  private var _graph                = Option.empty[Graph]
  private val codeFileNamePairs     = mutable.ArrayBuffer.empty[(String, Path)]
  private var fileNameCounter       = 0
  protected var _withPostProcessing = false

  @nowarn
  protected def codeFilePreProcessing(codeFile: Path): Unit = {}

  @nowarn
  protected def codeDirPreProcessing(rootFile: Path, codeFiles: List[Path]): Unit = {}

  protected def applyPasses(): Unit

  protected def applyPostProcessingPasses(): Unit = {}

  def moreCode(code: String): this.type = {
    moreCode(code, s"Test$fileNameCounter$fileSuffix")
    fileNameCounter += 1
    this
  }

  def moreCode(code: String, fileName: String): this.type = {
    checkGraphEmpty()
    codeFileNamePairs.append((code, Paths.get(fileName)))
    this
  }

  def withConfig(config: X2CpgConfig[_]): this.type = {
    setConfig(config)
    this
  }

  def withPostProcessingPasses(value: Boolean = true): this.type = {
    _withPostProcessing = value
    this
  }

  private def checkGraphEmpty(): Unit = {
    if (_graph.isDefined) {
      throw new RuntimeException("Modifying test data is not allowed after accessing graph.")
    }
  }

  private def codeToFileSystem(): Path = {
    val tmpDir = Files.createTempDirectory("x2cpgTestTmpDir")
    val codeFiles = codeFileNamePairs.map { case (code, fileName) =>
      if (fileName.getParent != null) {
        Files.createDirectories(tmpDir.resolve(fileName.getParent))
      }
      val codeAsBytes = code.getBytes(StandardCharsets.UTF_8)
      val codeFile    = tmpDir.resolve(Paths.get(fileName.toString))
      Files.write(codeFile, codeAsBytes)
      codeFilePreProcessing(codeFile)
      codeFile
    }.toList
    codeDirPreProcessing(tmpDir, codeFiles)
    tmpDir
  }

  private def deleteDir(dir: Path): Unit = {
    Files
      .walk(dir)
      .sorted(Comparator.reverseOrder[Path]())
      .forEach(Files.delete(_))
  }

  override def graph: Graph = {
    if (_graph.isEmpty) {
      val codeDir = codeToFileSystem()
      try {
        _graph = Option(execute(codeDir.toFile).graph)
        applyPasses()
        if (_withPostProcessing) applyPostProcessingPasses()
      } finally {
        deleteDir(codeDir)
      }
    }
    _graph.get
  }

  override def close(): Unit = {
    _graph.foreach(_.close())
  }
}
