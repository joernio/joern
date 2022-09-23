package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.passes.DiffGraph
import overflowdb.traversal._

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraph.Builder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasStoreMethod {

  override def store()(implicit diffBuilder: DiffGraph.Builder): Unit =
    traversal.sideEffect(storeRecursively).iterate()

  private def storeRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraph.Builder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Traversal[String] = traversal.map(_.label)
}
