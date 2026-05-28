package io.joern.x2cpg.passes.base

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

/** Old CPGs use the `order` field to indicate the parameter index while newer CPGs use the `parameterIndex` field. This
  * pass checks whether `parameterIndex` is not set for any parameter in a method, in which case the value of `order` is
  * copied over for all parameters of that method.
  *
  * If at least one parameter in a method already has an explicit index set, this is treated as a new-style CPG where
  * some parameters may intentionally have no index (e.g. keyword-only parameters in Python). In that case, the pass
  * leaves all parameters in that method unchanged.
  */
class ParameterIndexCompatPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    cpg.method.foreach { method =>
      val params        = method.parameter.l
      val anyIndexIsSet = params.exists(param => param.index.exists(index => index != -1))
      if (!anyIndexIsSet) {
        // Old-style CPG: no parameter in this method has an explicit index. copy order for all.
        params.foreach { param =>
          diffGraph.setNodeProperty(param, PropertyNames.Index, param.order)
        }
      }
      // If at least one parameter already has an explicit index, this is a new-style CPG.
      // Parameters with index = None are intentionally unindexed (e.g. keyword-only params);
      // leave them as None.
    }
  }

}
