package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.swiftsrc2cpg.SwiftSrc2Cpg
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, TestCpg}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DataFlowTestCpg extends TestCpg with SwiftSrc2CpgFrontend {
  override val fileSuffix: String = ".swift"

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    val context = new LayerCreatorContext(this)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    SwiftSrc2Cpg.postProcessingPasses(this).foreach(_.createAndApply())
  }

}

class DataFlowCodeToCpgSuite extends Code2CpgFixture(() => new DataFlowTestCpg()) {

  implicit var context: EngineContext = EngineContext()

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement: Option[Integer]) =>
      (firstElement, secondElement.get)
    }
}
