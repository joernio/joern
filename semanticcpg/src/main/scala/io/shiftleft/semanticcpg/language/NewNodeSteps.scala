package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal._

trait HasPersistMethod {
  def persist()(implicit diffBuilder: DiffGraphBuilder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasPersistMethod {

  override def persist()(implicit diffBuilder: DiffGraphBuilder): Unit =
    traversal.sideEffect(persistRecursively).iterate()

  private def persistRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraphBuilder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Traversal[String] = traversal.map(_.label)
}
