package io.joern.jssrc2cpg.datastructures

import io.joern.x2cpg.datastructures.Stack._

class LayeredStack[ElementType] {

  private val stack = new Stack[StackElement]()

  private case class StackElement(elements: List[ElementType] = List.empty) {
    def addNode(element: ElementType): StackElement = {
      StackElement(element :: elements)
    }
  }

  def pushLayer(): Unit = {
    stack.push(StackElement())
  }

  def popLayer(): Unit = {
    stack.pop()
  }

  def store(node: ElementType): Unit = {
    val head = stack.head.addNode(node)
    popLayer()
    stack.push(head)
  }

  def getTopElements: List[ElementType] = stack.head.elements

  def numberOfLayers: Int = stack.size

}
