package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

trait HasStoreMethod {
  def store()(implicit diffBuilder: DiffGraphBuilder): Unit
}

class NewNodeSteps[A <: NewNode](val traversal: Iterator[A]) extends HasStoreMethod {

  override def store()(implicit diffBuilder: DiffGraphBuilder): Unit =
    traversal.sideEffect(storeRecursively).iterate()

  private def storeRecursively(newNode: NewNode)(implicit diffBuilder: DiffGraphBuilder): Unit = {
    diffBuilder.addNode(newNode)
  }

  def label: Iterator[String] = traversal.map(_.label)
}
