package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.Implicits.IterableOnceDeco
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.nodemethods.AstNodeMethods.lastExpressionInBlock
import io.shiftleft.semanticcpg.utils.MemberAccess

class AstNodeMethods(val node: AstNode) extends AnyVal with NodeExtension {

  /** Indicate whether the AST node represents a control structure, e.g., `if`, `for`, `while`.
    */
  def isControlStructure: Boolean = node.isInstanceOf[ControlStructure]

  def isIdentifier: Boolean = node.isInstanceOf[Identifier]

  def isImport: Boolean = node.isInstanceOf[Import]

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

  /** Direct children of node in the AST. Siblings are ordered by their `order` fields
    */
  def astChildren: Iterator[AstNode] =
    node._astOut.cast[AstNode].sortBy(_.order).iterator

  /** Siblings of this node in the AST, ordered by their `order` fields
    */
  def astSiblings: Iterator[AstNode] =
    astParent.astChildren.filter(_ != node)

  /** Nodes of the AST rooted in this node, including the node itself.
    */
  def ast: Iterator[AstNode] =
    Iterator.single(node).ast

  /** Textual representation of AST node
    */
  def repr: String =
    node match {
      case method: Method                             => method.name
      case member: Member                             => member.name
      case methodReturn: MethodReturn                 => methodReturn.code
      case expr: Expression                           => expr.code
      case call: CallRepr if !call.isInstanceOf[Call] => call.code
    }

  def statement: AstNode =
    statementInternal(node, _.parentExpression.get)

  @scala.annotation.tailrec
  private def statementInternal(node: AstNode, parentExpansion: Expression => Expression): AstNode = {

    node match {
      case node: Identifier => parentExpansion(node)
      case node: MethodRef  => parentExpansion(node)
      case node: TypeRef    => parentExpansion(node)
      case node: Literal    => parentExpansion(node)

      case member: Member          => member
      case node: MethodParameterIn => node.method

      case node: MethodParameterOut => node.method.methodReturn

      case node: Call if MemberAccess.isGenericMemberAccessName(node.name) =>
        parentExpansion(node)

      case node: CallRepr     => node
      case node: MethodReturn => node
      case block: Block       =>
        // Just taking the lastExpressionInBlock is not quite correct because a BLOCK could have
        // different return expressions. So we would need to expand via CFG.
        // But currently the frontends do not even put the BLOCK into the CFG so this is the best
        // we can do.
        statementInternal(lastExpressionInBlock(block).get, identity)
      case node: Expression => node
    }
  }

}

object AstNodeMethods {

  private def lastExpressionInBlock(block: Block): Option[Expression] =
    block._astOut
      .collect {
        case node: Expression if !node.isInstanceOf[Local] && !node.isInstanceOf[Method] => node
      }
      .toVector
      .sortBy(_.order)
      .lastOption

}
