package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.JavaIteratorDeco
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class AstNodeMethods(val node: AstNode) extends AnyVal with NodeExtension {

  /** Indicate whether the AST node represents a control structure, e.g., `if`, `for`, `while`.
    */
  def isControlStructure: Boolean = node.isInstanceOf[ControlStructure]

  def isComment: Boolean = node.isInstanceOf[Comment]

  def isIdentifier: Boolean = node.isInstanceOf[Identifier]

  def isFieldIdentifier: Boolean = node.isInstanceOf[FieldIdentifier]

  def isFile: Boolean = node.isInstanceOf[File]

  def isReturn: Boolean = node.isInstanceOf[Return]

  def isLiteral: Boolean = node.isInstanceOf[Literal]

  def isLocal: Boolean = node.isInstanceOf[Local]

  def isCall: Boolean = node.isInstanceOf[Call]

  def isExpression: Boolean = node.isInstanceOf[Expression]

  def isMember: Boolean = node.isInstanceOf[Member]

  def isMethodRef: Boolean = node.isInstanceOf[MethodRef]

  def isMethod: Boolean = node.isInstanceOf[Method]

  def isModifier: Boolean = node.isInstanceOf[Modifier]

  def isNamespaceBlock: Boolean = node.isInstanceOf[NamespaceBlock]

  def isBlock: Boolean = node.isInstanceOf[Block]

  def isParameter: Boolean = node.isInstanceOf[MethodParameterIn]

  def isTypeDecl: Boolean = node.isInstanceOf[TypeDecl]

  def depth: Int = depth(_ => true)

  /** The depth of the AST rooted in this node. Upon walking the tree to its leaves, the depth is only increased for
    * nodes where `p(node)` is true.
    */
  def depth(p: AstNode => Boolean): Int = {
    val additionalDepth = if (p(node)) { 1 }
    else { 0 }

    val childDepths = node.astChildren.map(_.depth(p)).l
    additionalDepth + (if (childDepths.isEmpty) {
                         0
                       } else {
                         childDepths.max
                       })
  }

  def astParent: AstNode =
    node._astIn.onlyChecked.asInstanceOf[AstNode]

  /** Nodes of the AST rooted in this node, including the node itself.
    */
  def ast: Traversal[AstNode] =
    Traversal.fromSingle(node).ast

}
