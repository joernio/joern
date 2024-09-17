package io.joern.dataflowengineoss.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics}
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

/** Extends the capabilities of the test CPG to handle the configuration of data-flow enhancements.
  */
trait SemanticTestCpg { this: TestCpg =>

  protected var _withOssDataflow                = false
  protected var _extraFlows                     = List.empty[FlowSemantic]
  protected implicit var context: EngineContext = EngineContext()

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

  /** Some frontends require OSS data-flow to execute after post-processing, so we choose to expose this method without
    * defining where it's executed.
    */
  def applyOssDataFlow(): Unit = {
    if (_withOssDataflow) {
      val context = new LayerCreatorContext(this)
      val options = new OssDataFlowOptions(extraFlows = _extraFlows)
      new OssDataFlow(options).run(context)
      this.context = EngineContext(FullNameSemantics.fromList(DefaultSemantics().elements ++ _extraFlows))
    }
  }

}

/** Allows the tests to make use of the data-flow engine and any additional semantics.
  */
trait SemanticCpgTestFixture(extraFlows: List[FlowSemantic] = List.empty) {

  implicit val context: EngineContext = EngineContext(
    FullNameSemantics.fromList(DefaultSemantics().elements ++ extraFlows)
  )

}
