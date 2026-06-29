package io.joern.x2cpg.internal

import io.joern.x2cpg.{Ast, AstCreatorBase}
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewCall}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

private[x2cpg] trait CallAstBuilder[Node, NodeProcessor] {
  this: AstCreatorBase[Node, NodeProcessor] =>

  /** Create an abstract syntax tree for a call, including CPG-specific edges required for arguments and the receiver.
    *
    * Our call representation is inspired by ECMAScript, that is, in addition to arguments, a call has a base and a
    * receiver. For languages other than Javascript, leave `receiver` empty for now.
    *
    * @param callNode
    *   the node that represents the entire call
    * @param arguments
    *   arguments (without the base argument (instance))
    * @param base
    *   the value to use as `this` in the method call.
    * @param receiver
    *   the object in which the property lookup is performed
    */
  def callAst(
    callNode: NewCall,
    arguments: Seq[Ast] = List(),
    base: Option[Ast] = None,
    receiver: Option[Ast] = None
  ): Ast = {
    setArgumentIndices(arguments)

    val baseRoot = base.flatMap(_.root).toList
    val bse      = base.getOrElse(Ast())
    baseRoot match {
      case List(x: ExpressionNew) =>
        x.argumentIndex = 0
      case _ =>
    }

    val receiverRoot = if (receiver.isEmpty && base.nonEmpty) {
      baseRoot
    } else {
      val receiverList = receiver.flatMap(_.root).toList
      receiverList match {
        case List(expr: ExpressionNew) =>
          expr.argumentIndex = -1
        case _ =>
      }
      receiverList
    }

    val rcvAst = receiver.getOrElse(Ast())

    Ast(callNode)
      .withChild(rcvAst)
      .withChild(bse)
      .withChildren(arguments)
      .withArgEdges(callNode, baseRoot)
      .withArgEdges(callNode, arguments.flatMap(_.root))
      .withReceiverEdges(callNode, receiverRoot)
  }

  def fieldAccessAst(
    originNode: Node,
    fieldIdentifierOrigin: Node,
    base: Ast,
    code: String,
    fieldName: String,
    fieldTypeFullName: String
  ): Ast = {
    val callNode_ = callNode(
      originNode,
      code,
      Operators.fieldAccess,
      Operators.fieldAccess,
      DispatchTypes.STATIC_DISPATCH,
      None,
      Some(fieldTypeFullName)
    )
    val fieldIdentifierNode_ = fieldIdentifierNode(fieldIdentifierOrigin, fieldName, fieldName)
    callAst(callNode_, Seq(base, Ast(fieldIdentifierNode_)))
  }
}
