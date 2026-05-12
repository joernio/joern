package io.joern.rust2cpg.testfixtures

import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.rust2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.Code2CpgFixture
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class DataFlowTestCpg extends RustDefaultTestCpg {
  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }
}

class DataFlowCodeToCpgSuite(noSysRoot: Boolean = true)
    extends Code2CpgFixture(() => new DataFlowTestCpg().withConfig(Config().withNoSysRoot(noSysRoot))) {

  protected implicit val context: EngineContext = EngineContext()

  override def code(code: String): DataFlowTestCpg = {
    super.code(code, (Paths.get("src") / "lib.rs").toString)
  }

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement) =>
      (firstElement, secondElement.getOrElse(-1))
    }
}
