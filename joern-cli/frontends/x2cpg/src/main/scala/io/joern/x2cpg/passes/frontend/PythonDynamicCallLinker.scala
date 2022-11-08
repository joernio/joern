package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._

/** Link python calls to methods only by their name(not full name)
  * @param cpg
  *   the target code property graph.
  */
class PythonDynamicCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val methodNameToNode = cpg.method.toList.groupBy(_.name)
    def calls            = cpg.call.where(_.dispatchType(DispatchTypes.DYNAMIC_DISPATCH.name()))
    for {
      call    <- calls
      methods <- methodNameToNode.get(call.name)
      method  <- methods
    } {
      dstGraph.addEdge(call, method, EdgeTypes.CALL)
      // If we can only find one name with the exact match then we can semi-confidently set it as the full name
      if (methods.size == 1)
        dstGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, method.fullName)
    }
  }

}
