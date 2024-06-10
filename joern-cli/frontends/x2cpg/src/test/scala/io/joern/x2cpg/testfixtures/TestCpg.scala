package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.utils.TestCodeWriter
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.Graph

import java.nio.file.{Files, Path}
import java.util.Comparator

// Lazily populated test CPG which is created upon first access to the underlying graph.
// The trait LanguageFrontend is mixed in and not property/field of this class in order
// to allow the configuration of language frontend specific properties on the CPG object.
abstract class TestCpg extends Cpg() with LanguageFrontend with TestCodeWriter {
  private var _graph                = Option.empty[Graph]
  protected var _withPostProcessing = false

  protected def applyPasses(): Unit

  protected def applyPostProcessingPasses(): Unit = {}

  override def moreCode(code: String, fileName: String): TestCpg.this.type = {
    checkGraphEmpty()
    super.moreCode(code, fileName)
  }

  def withConfig(config: X2CpgConfig[?]): this.type = {
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

  private def deleteDir(dir: Path): Unit = {
    Files
      .walk(dir)
      .sorted(Comparator.reverseOrder[Path]())
      .forEach(Files.delete(_))
  }

  override def graph: Graph = {
    if (_graph.isEmpty) {
      val codeDir = writeCode(fileSuffix)
      try {
        _graph = Option(execute(codeDir.toFile).graph)
        applyPasses()
        if (_withPostProcessing) applyPostProcessingPasses()
      } finally {
        cleanupOutput()
      }
    }
    _graph.get
  }

  override def close(): Unit = {
    _graph.foreach(_.close())
  }
}
