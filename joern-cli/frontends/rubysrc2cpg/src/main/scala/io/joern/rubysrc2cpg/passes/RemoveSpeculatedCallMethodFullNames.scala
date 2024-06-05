package io.joern.rubysrc2cpg.passes

import io.shiftleft.codepropertygraph.generated.{Cpg, DispatchTypes, EdgeTypes, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate

class RemoveSpeculatedCallMethodFullNames(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    cpg.call.dispatchTypeExact(DispatchTypes.DYNAMIC_DISPATCH).foreach { call =>
      builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, XDefines.DynamicCallUnknownFullName)
      builder.setNodeProperty(call, PropertyNames.TYPE_FULL_NAME, XDefines.Any)
      call.outE(EdgeTypes.CALL).foreach { calleeEdge =>
        builder.removeEdge(calleeEdge)
      }
    }
  }

}
