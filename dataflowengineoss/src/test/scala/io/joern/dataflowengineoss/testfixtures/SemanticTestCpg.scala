package io.joern.dataflowengineoss.testfixtures

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics, Semantics}
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

/** Extends the capabilities of the test CPG to handle the configuration of data-flow enhancements.
  */
trait SemanticTestCpg { this: TestCpg =>

  protected var _withOssDataflow                = false
  protected var _semantics: Semantics           = DefaultSemantics()
  protected implicit var context: EngineContext = EngineContext()

  /** Allows one to enable data-flow analysis capabilities to the TestCpg.
    */
  def withOssDataflow(value: Boolean = true): this.type = {
    _withOssDataflow = value
    this
  }

  /** Allows one to provide custom semantics to the TestCpg. */
  def withSemantics(value: Semantics): this.type = {
    _semantics = value
    this
  }

  /** Some frontends require OSS data-flow to execute after post-processing, so we choose to expose this method without
    * defining where it's executed.
    */
  def applyOssDataFlow(): Unit = {
    if (_withOssDataflow) {
      val context  = new LayerCreatorContext(this)
      val options  = new OssDataFlowOptions(semantics = _semantics)
      val dataflow = new OssDataFlow(options)
      dataflow.run(context)
      this.context = EngineContext(dataflow.semantics)
    }
  }

}

/** Allows the tests to make use of the data-flow engine and any additional semantics.
  */
trait SemanticCpgTestFixture(semantics: Semantics = DefaultSemantics()) {

  implicit val context: EngineContext = EngineContext(semantics)

}
