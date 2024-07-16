package io.joern.x2cpg.testfixtures

import flatgraph.Graph
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.util.Using

object EmptyGraphFixture {
  def apply[T](fun: Graph => T): T =
    Using.resource(Cpg.empty.graph)(fun)
}
