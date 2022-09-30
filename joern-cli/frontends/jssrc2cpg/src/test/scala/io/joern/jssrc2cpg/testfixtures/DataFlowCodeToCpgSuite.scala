package io.joern.jssrc2cpg.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

class DataFlowCodeToCpgSuite extends JsSrc2CpgSuite {

  implicit var semantics: Semantics = DefaultSemantics()

  implicit var context: EngineContext = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    context = EngineContext(semantics)
  }

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
  }

  protected def flowToResultPairs(path: Path): List[(String, Integer)] =
    path.resultPairs().collect { case (firstElement: String, secondElement: Option[Integer]) =>
      (firstElement, secondElement.get)
    }
}
