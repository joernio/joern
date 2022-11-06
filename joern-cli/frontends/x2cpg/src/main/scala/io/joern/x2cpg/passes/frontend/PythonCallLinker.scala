package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._

// link python calls to methods only by their name(not full name)
class PythonCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  override def run(dstGraph: DiffGraphBuilder): Unit = {
    val methodNameToNode = cpg.method.toList.groupBy(_.name)
    for {
      call    <- cpg.call
      methods <- methodNameToNode.get(call.name)
      method  <- methods
    } dstGraph.addEdge(call, method, EdgeTypes.CALL)
  }

}
