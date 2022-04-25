package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.Graph

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

// Lazily populated test CPG which is created upon first access to the underlying graph.
class TestCpg(frontend: LanguageFrontend, registerCleanup: (Path, TestCpg) => Unit) extends Cpg() {
  private var _graph = Option.empty[Graph]
  private val codeFileNamePairs = mutable.ArrayBuffer.empty[(String, Path)]
  private var fileNameCounter = 0

  def moreCode(code: String): this.type = {
    val result = moreCode(code, s"Test$fileNameCounter.java")
    fileNameCounter += 1
    result
  }

  def moreCode(code: String, fileName: String): this.type = {
    checkGraphEmpty()
    codeFileNamePairs.append((code, Paths.get(fileName)))
    this
  }

  private def checkGraphEmpty(): Unit = {
    if (_graph.isDefined) {
      throw new RuntimeException("Modifying test data is not allowed after accessing graph.")
    }
  }

  private def codeToFileSystem(): Path = {
    val tmpDir = Files.createTempDirectory("x2cpgTestTmpDir")
    codeFileNamePairs.foreach{ case (code, fileName) =>
      if (fileName.getParent != null) {
        Files.createDirectories(tmpDir.resolve(fileName.getParent))
      }
      Files.writeString(tmpDir.resolve(Paths.get(fileName.toString)), code)
    }
    tmpDir
  }

  override def graph: Graph = {
    if (_graph.isEmpty) {
      val codeDir = codeToFileSystem()
      registerCleanup(codeDir, this)
      _graph = Some(frontend.execute(codeDir.toFile).graph)
      X2Cpg.applyDefaultOverlays(this)
    }
    _graph.get
  }

  override def close(): Unit = {
    _graph.foreach(_.close())
  }
}
