package io.joern.dataflowengineoss.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

/** Extends the capabilities of the test CPG to handle the configuration of data-flow enhancements.
  */
trait SemanticTestCpg { this: TestCpg =>

  private var _withOssDataflow                = false
  private var _extraFlows                     = List.empty[FlowSemantic]
  private implicit var context: EngineContext = EngineContext()

  /** Allows one to enable data-flow analysis capabilities to the TestCpg.
    */
  def withOssDataflow(value: Boolean = true): this.type = {
    _withOssDataflow = value
    this
  }

  /** Allows one to add additional semantics to the engine context during PDG creation.
    */
  def withExtraFlows(value: List[FlowSemantic] = List.empty): this.type = {
    _extraFlows = value
    this
  }

  override def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
    if (_withOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions(extraFlows = _extraFlows)
      new OssDataFlow(options).run(context)
      this.context = EngineContext(Semantics.fromList(DefaultSemantics().elements ++ _extraFlows))
    }
  }

}

/** Allows the tests to make use of the data-flow engine and any additional semantics.
  */
trait SemanticCpgTestFixture(extraFlows: List[FlowSemantic] = List.empty) {

  implicit val context: EngineContext = EngineContext(Semantics.fromList(DefaultSemantics().elements ++ extraFlows))

}
