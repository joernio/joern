package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn.PropertyDefaults
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

/** Old CPGs use the `order` field to indicate the parameter index while newer CPGs use the `parameterIndex` field. This
  * pass checks whether `parameterIndex` is not set, in which case the value of `order` is copied over.
  */
class ParameterIndexCompatPass(cpg: Cpg) extends SimpleCpgPass(cpg) {
  override def run(diffGraph: BatchedUpdate.DiffGraphBuilder): Unit = {
    cpg.parameter.foreach { param =>
      if (param.index == PropertyDefaults.Index) {
        diffGraph.setNodeProperty(param, PropertyNames.INDEX, param.order)
      }
    }
  }
}
