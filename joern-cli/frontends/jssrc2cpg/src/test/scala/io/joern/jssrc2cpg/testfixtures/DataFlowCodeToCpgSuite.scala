package io.joern.jssrc2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DataFlowCodeToCpgSuite extends JsSrc2CpgSuite {

  implicit var context: EngineContext = EngineContext()

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    JsSrc2Cpg.postProcessingPasses(cpg).foreach(_.createAndApply())
  }

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement: Option[Integer]) =>
      (firstElement, secondElement.get)
    }
}
