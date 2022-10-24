package io.joern.pysrc2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.pysrc2cpg.Py2CpgOnFileSystem.buildCpg
import io.joern.x2cpg.passes.frontend.PythonCallLinker
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend, TestCpg}
import io.joern.x2cpg.{X2Cpg, X2CpgConfig}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.File
import java.nio.file.Paths

trait PythonFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".py"

  override def execute(sourceCodeFile: File): Cpg = {
    val config = Py2CpgOnFileSystemConfig(Paths.get(X2CpgConfig.defaultOutputPath), sourceCodeFile.toPath, None)
    val cpg    = buildCpg(config)
    new PythonCallLinker(cpg).createAndApply()
    cpg
  }
}

class PySrcTestCpg extends TestCpg with PythonFrontend {
  private var _withOssDataflow = false

  def withOssDataflow(value: Boolean = true): this.type = {
    _withOssDataflow = value
    this
  }

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)

    if (_withOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }
}

class PySrc2CpgFixture(withOssDataflow: Boolean = false)
    extends Code2CpgFixture(() => new PySrcTestCpg().withOssDataflow(withOssDataflow)) {

  implicit val context: EngineContext = EngineContext()

}
