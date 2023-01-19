package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.parser.FileDefaults
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, TestCpg}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DataFlowTestCpg extends TestCpg with C2CpgFrontend {
  override val fileSuffix: String = FileDefaults.C_EXT

  override protected def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)

    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
  }
}

class DataFlowCodeToCpgSuite extends Code2CpgFixture(() => new DataFlowTestCpg()) {

  protected implicit val context: EngineContext = EngineContext()

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement: Option[Integer]) =>
      (firstElement, secondElement.getOrElse(-1))
    }

}
