package io.joern.x2cpg.datastructures

import scala.collection.mutable

object Stack {

  type Stack[StackElement] = mutable.ListBuffer[StackElement]

  implicit class StackWrapper[StackElement](val parentStack: Stack[StackElement]) extends AnyVal {
    def push(parent: StackElement): Unit = {
      parentStack.prepend(parent)
    }

    def pop(): Unit = {
      parentStack.remove(0)
    }
  }

}
