package io.shiftleft.pythonparser

import scala.collection.mutable

object IndentStack {
  val SPACE = 0
  val TAB= 1
}

// Keeps track of indentation by storing a chain of space and tab
// markers in an array buffer representing the stack. Each entry
// in this stack also has a "code level" marker which indicates
// whether there was any code on a certain indentation level.
class IndentStack {
  private val stack = mutable.ArrayBuffer.empty[Int]

  // Valid values for indentKind are IndentStack.SPACE and
  // IndentStack.TAB.
  def push(indentKind: Int): Unit = {
    stack.append(indentKind)
  }

  def pop(): Int = {
    stack.remove(stack.size - 1)
  }

  def markCodeLevel(): Unit = {
    val stackTopValue = stack.remove(stack.size - 1)
    val valueWithLevel = stackTopValue | (1 << 16)
    stack.append(valueWithLevel)
  }

  def isCodeLevel(index: Int): Boolean = {
    val stackValue = stack.apply(index)
    val isLevel = (stackValue & (1 << 16)) != 0
    isLevel
  }

  def getKind(index: Int): Int = {
    val stackValue = stack.apply(index)
    val indentKind = stackValue & ~(1 << 16)
    indentKind
  }

  def size(): Int = {
    stack.size
  }
}
