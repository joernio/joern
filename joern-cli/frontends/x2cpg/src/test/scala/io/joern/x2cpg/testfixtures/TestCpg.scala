package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.Graph

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.collection.mutable

// Lazily populated test CPG which is created upon first access to the underlying graph.
class TestCpg(frontend: LanguageFrontend, fixture: Code2CpgFixture) extends Cpg() {
  private var _graph            = Option.empty[Graph]
  private val codeFileNamePairs = mutable.ArrayBuffer.empty[(String, Path)]
  private var fileNameCounter   = 0

  def moreCode(code: String): TestCpg = {
    val result = moreCode(code, s"Test$fileNameCounter${frontend.fileSuffix}")
    fileNameCounter += 1
    result
  }

  def moreCode(code: String, fileName: String): TestCpg = {
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
    codeFileNamePairs.foreach { case (code, fileName) =>
      if (fileName.getParent != null) {
        Files.createDirectories(tmpDir.resolve(fileName.getParent))
      }
      val codeAsBytes = code.getBytes(StandardCharsets.UTF_8)
      Files.write(tmpDir.resolve(Paths.get(fileName.toString)), codeAsBytes)
    }
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
        _graph = Some(frontend.execute(codeDir.toFile).graph)
        fixture.applyPasses(this)
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
