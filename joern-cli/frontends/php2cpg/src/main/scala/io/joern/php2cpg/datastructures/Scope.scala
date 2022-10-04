package io.joern.php2cpg.datastructures

import io.joern.x2cpg.datastructures.{Scope => X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLocal, NewMethod, NewNode}

import scala.collection.mutable

class Scope extends X2CpgScope[String, NewNode, NewNode] {

  private var localStack: List[mutable.ArrayBuffer[NewLocal]] = Nil

  override def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case _: NewMethod => localStack = mutable.ArrayBuffer[NewLocal]() :: localStack
      case _            => // Nothing to do here
    }
    super.pushNewScope(scopeNode)
  }

  override def popScope(): Option[NewNode] = {
    val poppedScope = super.popScope()

    poppedScope match {
      case Some(_: NewMethod) =>
        // TODO This is unsafe to catch errors for now.
        localStack = localStack.tail
      case _ => // Nothing to do here
    }

    poppedScope
  }

  override def addToScope(identifier: String, variable: NewNode): NewNode = {
    variable match {
      case local: NewLocal =>
        // TODO This is unsafe to catch errors for now.
        localStack.head.addOne(local)
      case _ => // Nothing to do here
    }
    super.addToScope(identifier, variable)
  }

  def getLocalsInScope: List[NewLocal] = localStack.headOption.map(_.toList).toList.flatten
}
