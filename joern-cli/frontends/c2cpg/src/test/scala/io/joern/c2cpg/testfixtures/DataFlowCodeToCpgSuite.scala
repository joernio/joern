package io.joern.c2cpg.testfixtures

import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class DataFlowCodeToCpgSuite extends CCodeToCpgSuite {

  protected implicit val context: EngineContext = EngineContext()

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
  }

  protected implicit def int2IntegerOption(x: Int): Option[Integer] = Some(x)

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = path.resultPairs()
}
