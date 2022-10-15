package io.joern.pysrc2cpg

import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

object PythonDataflowContext {
  def buildCpg(code: String, file: String = "test.py"): Cpg = {
    val context = new PythonDataflowContext()
    context.addSource(code, file)
    context.buildCpg
  }
}

class PythonDataflowContext extends Py2CpgTestContext {
  override def buildCpg: Cpg = {
    val cpg = super.buildCpg
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    cpg
  }
}
