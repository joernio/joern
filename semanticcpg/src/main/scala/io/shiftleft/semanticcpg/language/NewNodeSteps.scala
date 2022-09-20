package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import overflowdb.traversal._
import overflowdb.BatchedUpdate.DiffGraphBuilder

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraphBuilder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasStoreMethod {

  override def store()(implicit diffBuilder: DiffGraphBuilder): Unit =
    traversal.sideEffect(storeRecursively).iterate()

  private def storeRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraphBuilder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Traversal[String] = traversal.map(_.label)
}
