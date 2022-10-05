package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.passes.DiffGraph
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.traversal._

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraph.Builder): Unit
}

trait HasStoreAndPersistMethods extends HasStoreMethod {
  def persist()(implicit diffBuilder: DiffGraphBuilder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Traversal[A]) extends HasStoreAndPersistMethods {

  override def store()(implicit diffBuilder: DiffGraph.Builder): Unit =
    traversal.sideEffect(storeRecursively).iterate()

  private def storeRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraph.Builder): Unit = {
    diffBuilder.addNode(newNode)
  }

  override def persist()(implicit diffBuilder: DiffGraphBuilder): Unit =
    traversal.sideEffect(persistRecursively).iterate()

  private def persistRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraphBuilder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Traversal[String] = traversal.map(_.label)
}
