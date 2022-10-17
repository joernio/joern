package io.joern.pysrc2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.pysrc2cpg.Py2CpgOnFileSystem.buildCpg
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.io.File
import java.nio.file.Paths

class PythonFrontend() extends LanguageFrontend {
  override val fileSuffix: String = ".py"

  override def execute(sourceCodeFile: File): Cpg = {
    val config = Py2CpgOnFileSystemConfig(Paths.get(X2CpgConfig.defaultOutputPath), sourceCodeFile.toPath, None)
    buildCpg(config)
  }
}

class PySrc2CpgFixture(withOssDataflow: Boolean = false) extends Code2CpgFixture(new PythonFrontend()) {

  implicit val context: EngineContext = EngineContext()

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)

    if (withOssDataflow) {
      val context = new LayerCreatorContext(cpg)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
  }

}
