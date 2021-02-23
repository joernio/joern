package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import scala.collection.mutable

class ContextStack {
  private sealed trait Context {
    val name: String
    val cpgNode: nodes.NewNode
    val order: AutoIncIndex
  }

  private class MethodContext(val name: String,
                              val cpgNode: nodes.NewNode,
                              val order: AutoIncIndex) extends Context {

  }

  private class ClassContext(val name: String,
                             val cpgNode: nodes.NewNode,
                             val order: AutoIncIndex) extends Context {

  }

  private class NamespaceBlockContext(val name: String,
                                      val cpgNode: nodes.NewNode,
                                      val order: AutoIncIndex) extends Context {

  }


  private val stack = mutable.Stack.empty[Context]

  def pushMethod(name: String, methodNode: nodes.NewMethod): Unit = {
    stack.push(new MethodContext(name, methodNode, new AutoIncIndex(1)))
  }

  def pushClass(name: String, classNode: nodes.NewTypeDecl): Unit = {
    stack.push(new ClassContext(name, classNode, new AutoIncIndex(1)))
  }

  def pushNamespaceBlock(namespaceBlock: nodes.NewNamespaceBlock): Unit = {
    stack.push(new NamespaceBlockContext("", namespaceBlock, new AutoIncIndex(1)))
  }

  def pop(): Unit = {
    stack.pop()
  }

  // Together with the file name this is used to compute full names.
  def qualName: String = {
    stack.filter { element =>
      element.isInstanceOf[MethodContext] || element.isInstanceOf[ClassContext]
    }
    .map(_.name).mkString(".")
  }

  def astParent: nodes.NewNode = {
    stack.head.cpgNode
  }

  def order: AutoIncIndex = {
    stack.head.order
  }

  def isClassContext: Boolean = {
    stack.head.isInstanceOf[ClassContext]
  }


}
