package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.passes.DiffGraph
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal._

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraphBuilder): Unit
}

trait HasPersistMethod {
  def persist()(implicit diffBuilder: DiffGraphBuilder): Unit
}

trait HasStoreAndPersistMethods extends HasStoreMethod with HasPersistMethod

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasStoreAndPersistMethods {

  override def store()(implicit diffBuilder: DiffGraphBuilder): Unit = persist()

  override def persist()(implicit diffBuilder: DiffGraphBuilder): Unit =
    traversal.sideEffect(persistRecursively).iterate()

  private def persistRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraphBuilder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Traversal[String] = traversal.map(_.label)
}
