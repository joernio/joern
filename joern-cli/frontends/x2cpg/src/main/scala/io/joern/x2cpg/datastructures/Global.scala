package io.joern.x2cpg.datastructures

import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import java.util.concurrent.ConcurrentHashMap

class Global {

  val usedTypes: ConcurrentHashMap[String, Boolean] = new ConcurrentHashMap()

  val nodesWithGenericTypes: ConcurrentHashMap[NewNode, CodeTree] = new ConcurrentHashMap()

}

case class TreeNode(value: String, children: List[TreeNode] = List.empty) {

  def withChildren(children: List[TreeNode]): TreeNode = this.copy(children = this.children ++ children)

  override def toString: String = value
}

abstract class CodeTree(val root: TreeNode) {

  protected val separator: String
  protected val lbracket: String
  protected val rbracket: String

  // Lazy load the code tree string
  private lazy val treeString = _toString(List(root))

  override def toString: String = treeString

  private def _toString(xs: List[TreeNode]): String = xs match {
    case head :: Nil if head.children.nonEmpty =>
      head.toString + lbracket + _toString(head.children) + rbracket
    case head :: next if head.children.nonEmpty =>
      head.toString + lbracket + _toString(head.children) + rbracket + separator + _toString(next)
    case head :: Nil  => head.toString
    case head :: next => head.toString + separator + _toString(next)
    case Nil          => ""
  }

}

final class JavaTree(root: TreeNode) extends CodeTree(root) {
  override protected val separator: String = ", "
  override protected val lbracket: String  = "<"
  override protected val rbracket: String  = ">"
}
