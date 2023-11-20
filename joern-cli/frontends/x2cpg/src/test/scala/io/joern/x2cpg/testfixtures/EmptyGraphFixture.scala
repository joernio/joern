package io.joern.x2cpg.testfixtures

import io.joern.odb2.Graph
import io.shiftleft.codepropertygraph.generated.v2.Cpg

object EmptyGraphFixture {
  def apply[T](fun: Graph => T): T = {
    val graph = Cpg.empty.graph
    try fun(graph)
    finally ??? // TODO get graph.close back graph.close()
  }
}
