package io.shiftleft.semanticcpg.testing

import io.shiftleft.codepropertygraph.generated.v2.nodes.StoredNode

trait DummyNodeImpl extends StoredNode {
  def propertiesMap: java.util.Map[String, Any] = ???
}